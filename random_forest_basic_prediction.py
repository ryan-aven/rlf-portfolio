# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 14:51:54 2020

@author: RAven
"""

import pandas as pd

#importing data set
merged_rlf = pd.read_csv("G:/Shared/Disaster_RECOVERY_Assistance/COVID-19/Ryan's Working Files/Research Project - RLFs/merged_data.csv", engine="python")

#dropping current loans
merged_rlf.drop(merged_rlf[merged_rlf.loan_status.isin(["delinquent", "current", "in default"])].index, inplace = True)

#bui