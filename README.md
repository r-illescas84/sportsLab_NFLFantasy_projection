# sportsLab NFL Fantasy Projection

Fantasy football analysis and projections built on [nflverse](https://nflverse.com) data.

The repository has two parts:

- **`weekly/`**: weekly projections. Predicts each player's receptions, receiving yards and receiving TDs for the upcoming week, converts them to fantasy points and compares them with FantasyPros. Currently covers WRs; TEs, RBs and QBs are next.
- **`draft/`**: season-long work for drafts: ADP data, custom player metrics and safest picks by round.

## Setup

```
pip install -r requirements.txt
```

## Weekly projections

```
weekly/
├── src/
│   ├── config.py       # all settings: season, week, position, features, model settings, paths
│   ├── features.py     # loads nflverse data and builds every feature (shared by training and prediction)
│   └── external.py     # FantasyPros projections and the injury report
├── notebooks/
│   ├── 01_build_training_data.ipynb
│   ├── 02_train_models.ipynb
│   ├── 03_predict_week.ipynb
│   └── 04_fantasy_points.ipynb
├── data/               # training table (git-ignored, rebuilt by notebook 01)
├── models/WR/          # one saved model per predicted stat, plus metrics.csv
└── outputs/WR/<year>/  # weekly predictions and fantasy points
```

### Every week
1. Set `WEEK` in `weekly/src/config.py`.
2. Run `03_predict_week`: predicts the top WRs on each team's depth chart and saves `outputs/WR/<year>/wrs_predictions_week<N>.csv`.
3. Run `04_fantasy_points`: adds half PPR and PPR points, FantasyPros' projection, the difference with ours and injury status, and saves `wrs_fantasy_week<N>.csv`.

A past week can be re-run to compare predictions with what happened: features only use games before the predicted week.

### Once per offseason
1. Run `01_build_training_data` to rebuild the training table with the new season.
2. Run `02_train_models` to retrain and save the models. It trains on every season before the last two, picks the number of trees on the second-to-last season (early stopping) and reports scores on the last season.

### How it works
- **Active games** come from snap counts: a game counts when the player took a snap, so games with 0 targets are included.
- **Features** are averages from games *before* the predicted one: the player's season, last 3 games and career averages, what their offense gained and what the opponent's defense allowed.
- **Training rows** are WR games where the player had at least 40% of offensive snaps in their previous game.
- **Models** are XGBoost, one per stat, with settings tuned per stat in `config.XGB_PARAMS`.

## Draft

```
draft/
├── data/               # FantasyPros ADP files (fantasypros_adp/) and the ADP tables built from them
├── notebooks/          # name_to_id -> ricky_general_metrics -> obj_metrics -> safest_picks_by_round
├── metrics/QB, TE, WR/ # custom metrics per position and the yearly tables with them
└── models/             # regression test
```

Some draft notebooks still use `nfl_data_py`, which is deprecated and whose seasonal/weekly stats downloads no longer work. They need to be moved to `nflreadpy` before they are re-run.
