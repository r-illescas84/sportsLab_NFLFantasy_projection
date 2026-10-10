"""Outside information shown next to the predictions: FantasyPros projections and the injury report."""
import re

import nflreadpy as nfr
import pandas as pd

from . import config

# FantasyPros team abbreviations that differ from nflverse
FANTASYPROS_TEAMS = {'LAR': 'LA', 'JAC': 'JAX'}


def _name_key(name):
    """Lowercase name without punctuation or suffixes, to match players when ids are missing."""
    name = re.sub(r'[^a-z ]', '', str(name).lower())
    return ' '.join(w for w in name.split() if w not in {'jr', 'sr', 'ii', 'iii', 'iv', 'v'})


def load_fantasypros_wr():
    """This week's FantasyPros PPR WR rankings with their projected PPR points, keyed by gsis player_id.

    Players are matched by id (DynastyProcess id table), then by name and team for ids missing from that table.
    """
    fp = nfr.load_ff_rankings(type='week').to_pandas()
    fp = fp[fp['page'] == 'ppr-wr']
    fp = fp.assign(team=fp['team'].replace(FANTASYPROS_TEAMS),
                   fp_opponent=fp['player_opponent_id'].replace(FANTASYPROS_TEAMS),
                   name_key=fp['player_name'].map(_name_key))

    ids = nfr.load_ff_playerids().to_pandas()[['fantasypros_id', 'gsis_id']].dropna()
    ids['fantasypros_id'] = ids['fantasypros_id'].astype('int64')
    ids = ids.drop_duplicates('fantasypros_id')
    fp = fp.merge(ids, on='fantasypros_id', how='left').rename(columns={'gsis_id': 'player_id'})

    cols = ['player_id', 'name_key', 'team', 'fp_opponent', 'rank', 'r2p_pts', 'scrape_date']
    return fp[cols].rename(columns={'rank': 'fantasypros_rank', 'r2p_pts': 'fantasypros_ppr'})


def add_fantasypros(predictions):
    """Adds FantasyPros rank and projected PPR points, matching by id first and by name and team second."""
    fp = load_fantasypros_wr()
    fp_cols = ['fantasypros_rank', 'fantasypros_ppr', 'fp_opponent']
    by_id = predictions.merge(fp.dropna(subset=['player_id'])[['player_id'] + fp_cols].drop_duplicates('player_id'),
                              on='player_id', how='left')
    missing = by_id['fantasypros_ppr'].isna()
    by_name = (predictions[missing]
               .assign(name_key=predictions.loc[missing, 'player_display_name'].map(_name_key))
               .merge(fp[['name_key', 'team'] + fp_cols].drop_duplicates(['name_key', 'team']),
                      left_on=['name_key', 'ball_team'], right_on=['name_key', 'team'], how='left'))
    by_id.loc[missing, fp_cols] = by_name[fp_cols].values
    return by_id, fp['scrape_date'].iloc[0]


def load_injury_status(year, week):
    """Latest injury report status for the week (Out, Doubtful, Questionable...), keyed by player_id."""
    injuries = nfr.load_injuries(seasons=[year]).to_pandas()
    injuries = injuries[injuries['week'] == week]
    injuries = injuries.rename(columns={'gsis_id': 'player_id', 'report_status': 'injury_status'})
    # Players who only appear in the practice report have no game status
    injuries = injuries.dropna(subset=['injury_status'])
    return injuries[['player_id', 'injury_status']].drop_duplicates('player_id', keep='last')
