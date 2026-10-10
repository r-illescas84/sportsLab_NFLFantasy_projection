"""Shared functions that load the data and build the feature table used for training and prediction."""
import numpy as np
import nflreadpy as nfr
import pandas as pd

from . import config

ID_COLS = ['player_id', 'player_display_name', 'position', 'season', 'week',
           'game_id', 'ball_team', 'opponent_team']
KEYS = ['player_id', 'season', 'week']


def load_snap_counts(years):
    """Regular season offensive snap counts for offensive players, keyed by gsis player_id."""
    snaps = nfr.load_snap_counts(seasons=years).to_pandas()
    snaps = snaps[(snaps['game_type'] == 'REG') & (snaps['position'].isin(config.OFFENSE_POSITIONS))]
    # Snap counts use Pro Football Reference ids, the stats use gsis ids
    ids = nfr.load_players().to_pandas()[['gsis_id', 'pfr_id']].dropna()
    snaps = snaps.merge(ids, left_on='pfr_player_id', right_on='pfr_id', how='inner')
    snaps = snaps.rename(columns={'gsis_id': 'player_id', 'player': 'player_display_name',
                                  'team': 'ball_team', 'opponent': 'opponent_team'})
    return snaps[ID_COLS + ['offense_snaps', 'offense_pct', 'st_snaps']]


def load_weekly(years):
    """Regular season stats for offensive players, one row per player per game they were active.

    A game counts when the player took a snap (snap counts) or recorded a stat (player stats).
    Active games without a stats row (e.g. a WR with no targets) get 0 for every stat.
    """
    stats = nfr.load_player_stats(seasons=years, summary_level='week').to_pandas()
    # Offensive players, plus anyone else with an offensive touch (lineman catches, fake punts) so team totals are complete
    offensive_touch = stats[['attempts', 'carries', 'targets']].fillna(0).sum(axis=1) > 0
    stats = stats[(stats['season_type'] == 'REG') &
                  (stats['position_group'].isin(config.OFFENSE_POSITIONS) | offensive_touch)]
    stats = stats.rename(columns={'team': 'ball_team'})[ID_COLS + config.PLAYER_STATS]

    snaps = load_snap_counts(years)
    weekly = stats.merge(snaps, on=KEYS, how='outer', suffixes=('', '_snaps'))
    # Snap-only rows have no ids from the stats, take them from the snap counts
    for col in ['player_display_name', 'position', 'game_id', 'ball_team', 'opponent_team']:
        weekly[col] = weekly[col].fillna(weekly[f'{col}_snaps'])
    weekly = weekly.drop(columns=[c for c in weekly.columns if c.endswith('_snaps') and c != 'offense_snaps'])
    weekly[config.PLAYER_STATS] = weekly[config.PLAYER_STATS].fillna(0)

    for col in ['ball_team', 'opponent_team']:
        weekly[col] = weekly[col].replace(config.TEAM_RENAMES)
    return weekly.sort_values(KEYS).reset_index(drop=True)


def add_prev_snap_pct(weekly):
    """Adds the player's offensive snap share in their previous game (NaN for their first game)."""
    weekly = weekly.sort_values(KEYS).reset_index(drop=True)
    weekly['prev_offense_pct'] = weekly.groupby('player_id')['offense_pct'].shift()
    return weekly


def _previous_games_avg(df, group_cols, stats):
    """Average of each stat over the earlier rows of the same group (df must be sorted by game order).

    Only rows before the current one are used, so a row for an upcoming game
    (stats still NaN) gets the same averages as a row for a game already played.
    """
    keys = [df[c] for c in group_cols]
    previous = df.groupby(keys)[stats].shift()
    prev_sum = previous.groupby(keys).cumsum()
    prev_cnt = previous.notna().groupby(keys).cumsum()
    return prev_sum / prev_cnt.replace(0, np.nan)


def player_season_avg(weekly, stats=config.PLAYER_STATS):
    """Adds each stat's average over the player's earlier games that season (NaN for the first game)."""
    weekly = weekly.sort_values(KEYS).reset_index(drop=True)
    season_avg = _previous_games_avg(weekly, ['player_id', 'season'], stats)
    return weekly.join(season_avg.add_suffix('_season_avg'))


def player_last_n_avg(weekly, stats=config.PLAYER_STATS, n=config.N_GAMES):
    """Adds each stat's average over the player's previous n games, across seasons (NaN for their first game).

    Players with fewer than n earlier games get the average of the games they have.
    """
    weekly = weekly.sort_values(KEYS).reset_index(drop=True)
    last_n = _previous_n_games_avg(weekly, 'player_id', stats, n)
    return weekly.join(last_n.add_suffix(f'_last{n}'))


def player_career_avg(weekly, stats=config.PLAYER_STATS):
    """Adds each stat's average over all the player's earlier games in the loaded years (NaN for the first game)."""
    weekly = weekly.sort_values(KEYS).reset_index(drop=True)
    career_avg = _previous_games_avg(weekly, ['player_id'], stats)
    return weekly.join(career_avg.add_suffix('_career'))


def _previous_n_games_avg(df, group_col, stats, n):
    """Average of each stat over the previous n rows of the same group, across seasons (df sorted by game order)."""
    previous = df.groupby(group_col)[stats].shift()
    return (previous.groupby(df[group_col])
            .rolling(n, min_periods=1).mean()
            .reset_index(level=0, drop=True)
            .sort_index())


def team_totals(weekly, team_col, stats=config.TEAM_STATS):
    """Sums the players' stats into one row per team per game, sorted by game order.

    team_col='ball_team' gives what the team gained on offense, 'opponent_team' what the team allowed on defense.
    An upcoming game (all stats NaN) stays NaN instead of becoming 0.
    """
    totals = (weekly.groupby(['game_id', team_col, 'season', 'week'])[stats]
              .sum(min_count=1)
              .reset_index())
    return totals.sort_values([team_col, 'season', 'week']).reset_index(drop=True)


def _team_features(weekly, team_col, prefix, stats, n):
    """Adds a team's season average and last n games average, from its earlier games, to each player row."""
    totals = team_totals(weekly, team_col, stats)
    season_avg = _previous_games_avg(totals, [team_col, 'season'], stats).add_suffix('_season_avg')
    last_n = _previous_n_games_avg(totals, team_col, stats, n).add_suffix(f'_last{n}')
    team = totals[['game_id', team_col]].join(season_avg.join(last_n).add_prefix(prefix))
    return weekly.merge(team, on=['game_id', team_col], how='left')


def offense_features(weekly, stats=config.TEAM_STATS, n=config.N_GAMES):
    """Adds what the player's team gained on offense: off_<stat>_season_avg and off_<stat>_last<n>."""
    return _team_features(weekly, 'ball_team', 'off_', stats, n)


def defense_features(weekly, stats=config.TEAM_STATS, n=config.N_GAMES):
    """Adds what the player's opponent allowed on defense: def_<stat>_season_avg and def_<stat>_last<n>."""
    return _team_features(weekly, 'opponent_team', 'def_', stats, n)


def load_week_games(year, week):
    """One row per team playing in a regular season week: game_id, ball_team, opponent_team and whether it was played."""
    schedule = nfr.load_schedules(seasons=[year]).to_pandas()
    schedule = schedule[(schedule['game_type'] == 'REG') & (schedule['week'] == week)]
    schedule = schedule.assign(played=schedule['home_score'].notna())
    home = schedule.rename(columns={'home_team': 'ball_team', 'away_team': 'opponent_team'})
    away = schedule.rename(columns={'away_team': 'ball_team', 'home_team': 'opponent_team'})
    cols = ['game_id', 'season', 'week', 'gameday', 'ball_team', 'opponent_team', 'played']
    return pd.concat([home[cols], away[cols]], ignore_index=True)


def load_upcoming_players(year, week):
    """Top config.DEPTH_CHART_WRS WRs on each team's latest depth chart, with their game that week.

    Teams on bye that week are left out.
    """
    depth = nfr.load_depth_charts(seasons=[year]).to_pandas()
    depth = depth[(depth['dt'] == depth['dt'].max()) & (depth['pos_abb'] == 'WR') &
                  (depth['pos_rank'] <= config.DEPTH_CHART_WRS)]
    players = depth.rename(columns={'gsis_id': 'player_id', 'player_name': 'player_display_name',
                                    'team': 'ball_team', 'pos_rank': 'depth_rank'})
    players = players[['player_id', 'player_display_name', 'ball_team', 'depth_rank']].drop_duplicates('player_id')
    players = players.merge(load_week_games(year, week), on='ball_team', how='inner')
    return players.assign(position='WR')


def build_feature_table(years, upcoming=None):
    """One row per player per active game with ids, targets and every candidate feature.

    Used for training (past seasons) and prediction (current season plus upcoming games),
    so both steps compute features the same way. Models pick their columns from config.MODEL_FEATURES.

    upcoming: rows for the games to predict (from load_upcoming_players). Games from that week on are
    dropped from the loaded data first, so the features only use earlier games even if some of the
    week's games were already played. The upcoming rows keep NaN stats.
    """
    weekly = load_weekly(years)
    if upcoming is not None:
        season, week = upcoming['season'].iloc[0], upcoming['week'].iloc[0]
        later = (weekly['season'] > season) | ((weekly['season'] == season) & (weekly['week'] >= week))
        weekly = pd.concat([weekly[~later], upcoming[ID_COLS]], ignore_index=True)
    weekly = add_prev_snap_pct(weekly)
    weekly = player_season_avg(weekly)
    weekly = player_last_n_avg(weekly)
    weekly = player_career_avg(weekly)
    weekly = offense_features(weekly)
    weekly = defense_features(weekly)
    return weekly


def select_training_rows(table, method='snap'):
    """WR rows used to train the models.

    'snap':  previous game offensive snap share >= config.MIN_PREV_SNAP_PCT (only uses earlier information)
    'yards': config.MIN_SEASON_REC_YARDS+ receiving yards that season (the old rule, for comparison)
    """
    wrs = table[table['position'] == 'WR']
    if method == 'snap':
        return wrs[wrs['prev_offense_pct'] >= config.MIN_PREV_SNAP_PCT]
    if method == 'yards':
        season_yards = wrs.groupby(['player_id', 'season'])['receiving_yards'].transform('sum')
        return wrs[season_yards >= config.MIN_SEASON_REC_YARDS]
    raise ValueError(f"method must be 'snap' or 'yards', got {method!r}")
