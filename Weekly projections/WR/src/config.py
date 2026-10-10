"""Settings shared by every step: season/week, training years, feature lists per model and file paths."""
from pathlib import Path

# ---------------------------------------------------------------------------
# Season and week
# ---------------------------------------------------------------------------
CURRENT_YEAR = 2026
WEEK = 5  # week to predict (features only use earlier weeks, so a past week can be re-run to backtest)

# Seasons used to train the models (current season excluded)
TRAIN_YEARS = list(range(2014, CURRENT_YEAR))

# ---------------------------------------------------------------------------
# Data rules
# ---------------------------------------------------------------------------
N_GAMES = 3  # window for the "last n games" averages

# Old team abbreviations mapped to the current ones
TEAM_RENAMES = {'STL': 'LA', 'OAK': 'LV', 'SD': 'LAC'}

# Positions kept from the weekly player stats and snap counts (both also include defense/special teams)
OFFENSE_POSITIONS = ['QB', 'RB', 'FB', 'WR', 'TE']

# Training rows: WR games where the player's offensive snap share in their previous game was at least this
MIN_PREV_SNAP_PCT = 0.40

# Alternative training filter (400+ receiving yards that season), kept to compare against in step 02
MIN_SEASON_REC_YARDS = 400

# Players predicted each week: top n WRs on each team's depth chart
DEPTH_CHART_WRS = 3

# ---------------------------------------------------------------------------
# Models
# ---------------------------------------------------------------------------
TARGETS = ['receptions', 'receiving_yards', 'receiving_tds']

# Stats averaged per player, offense and defense (season, last n games, career)
PLAYER_STATS = ['receptions', 'receiving_yards', 'receiving_tds', 'target_share',
                'carries', 'rushing_yards', 'rushing_tds']
TEAM_STATS = ['receptions', 'receiving_yards', 'receiving_tds',
              'carries', 'rushing_yards', 'rushing_tds']

last_n = f'last{N_GAMES}'

BASE_FEATURES = [
    # Receiver
    'receptions_season_avg', 'receiving_yards_season_avg', 'receiving_tds_season_avg', 'target_share_season_avg',
    f'receptions_{last_n}', f'receiving_yards_{last_n}', f'receiving_tds_{last_n}', f'target_share_{last_n}',
    'receptions_career', 'receiving_yards_career', 'receiving_tds_career', 'target_share_career',

    # Opponent defense
    'def_receptions_season_avg', 'def_receiving_yards_season_avg', 'def_receiving_tds_season_avg',
    f'def_receptions_{last_n}', f'def_receiving_yards_{last_n}', f'def_receiving_tds_{last_n}',

    # Receiver's offense
    'off_receptions_season_avg', 'off_receiving_yards_season_avg', 'off_receiving_tds_season_avg',
    f'off_receptions_{last_n}', f'off_receiving_yards_{last_n}', f'off_receiving_tds_{last_n}', f'off_carries_{last_n}',
]

# Features used by each model (same for all three for now)
MODEL_FEATURES = {
    'receptions': BASE_FEATURES,
    'receiving_yards': BASE_FEATURES,
    'receiving_tds': BASE_FEATURES,
}

# XGBoost settings per target, tuned on VALIDATION_SEASON.
# Without 'n_estimators', early stopping on VALIDATION_SEASON picks the number of trees.
XGB_PARAMS = {
    # Poisson suits a count
    'receptions': {
        'objective': 'count:poisson',
        'max_depth': 3,
        'min_child_weight': 20,
        'learning_rate': 0.03,
        'subsample': 0.8,
        'colsample_bytree': 0.8,
        'random_state': 84,
    },
    # Squared error: yards can be negative, so Poisson doesn't apply
    'receiving_yards': {
        'objective': 'reg:squarederror',
        'max_depth': 2,
        'min_child_weight': 20,
        'learning_rate': 0.03,
        'subsample': 0.8,
        'colsample_bytree': 0.8,
        'random_state': 84,
    },
    # Poisson suits a count that is mostly 0, shallow trees with large leaves avoid memorising noise
    'receiving_tds': {
        'objective': 'count:poisson',
        'max_depth': 3,
        'min_child_weight': 100,
        'learning_rate': 0.03,
        'subsample': 0.8,
        'colsample_bytree': 0.8,
        'random_state': 84,
    },
}
EARLY_STOPPING_ROUNDS = 100  # stop when the validation score hasn't improved for this many trees
MAX_TREES = 3000

# Season held out to compare models (trained on the earlier seasons, tested on this one)
TEST_SEASON = TRAIN_YEARS[-1]
# Season used to choose settings and the number of trees (never used to report scores)
VALIDATION_SEASON = TEST_SEASON - 1

# Rows used to train the final models: 'snap' or 'yards' (see features.select_training_rows)
TRAINING_METHOD = 'snap'

# ---------------------------------------------------------------------------
# Fantasy scoring (receiving only, the models don't predict rushing)
# ---------------------------------------------------------------------------
SCORING = {
    'half_ppr': {'receptions': 0.5, 'receiving_yards': 0.1, 'receiving_tds': 6},
    'ppr': {'receptions': 1.0, 'receiving_yards': 0.1, 'receiving_tds': 6},
}

# ---------------------------------------------------------------------------
# Paths (relative to the WR folder, so they work from any working directory)
# ---------------------------------------------------------------------------
WR_DIR = Path(__file__).resolve().parent.parent
DATA_DIR = WR_DIR / 'data'
MODELS_DIR = WR_DIR / 'models'
OUTPUTS_DIR = WR_DIR / 'outputs'

TRAINING_DATA = DATA_DIR / 'training_data.parquet'


MODEL_METRICS = MODELS_DIR / 'metrics.csv'


def model_path(target):
    return MODELS_DIR / f'xgb_{target}.json'


def predictions_path(year, week):
    return OUTPUTS_DIR / str(year) / f'wrs_predictions_week{week}.csv'


def fantasy_path(year, week):
    return OUTPUTS_DIR / str(year) / f'wrs_fantasy_week{week}.csv'
