from flask_restful import Resource
import pandas as pd
roll_calls_url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv'
roll_call = pd.read_csv(roll_calls_url,  dtype={'rcid':'object'})
class GetDescription(Resource):
    def get(self, rcid):
        rows = roll_call[roll_call.rcid == rcid]
        if rows.shape[0] > 0:
            description = rows.iloc[0]["descr"]
            return {"desc": description}, 201
        else:
            description = "NOT AVAILABLE"
            return {"desc": description}, 404