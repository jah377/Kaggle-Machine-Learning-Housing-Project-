'''
Module contains helper functions used throughout preprocessing for models
'''
import numpy as np
import pandas as pd
from scipy import stats



# ========== Quantify n/perc of missing values ==========
def missValue_quant(df):
    """
    Returns df of features missing data (count and percentage)
    """
    data = {"n_miss": df.isna().sum(), "perc_miss": df.isna().mean()} #dict
    missing = pd.DataFrame(data=data, index=list(df.columns.values)).sort_values(ascending=False, by="n_miss") #dataframe
    missing = missing.loc[missing.n_miss > 0] #isolate features with missing values

    if missing.shape[0] > 0: 
        return missing 
    else:
        return print('No missing values')



# ========== Replace NA with custom string ==========
def fillna_wString(df, replacement_dict):
    """
    Replaces pseudo-missing data with value determine in dictionary
    """
    try: 
        [df[feat].fillna(replacement_dict[feat], inplace=True) for feat in replacement_dict]
        print('Features successfully imputed with string')
    except ValueError:
        print('Features in dictionary not found in dataframe')



# ========== Determine/Replace Actual-Missing Values from Pseudo-Missing Values ==========
def id_actual_NA(dataframe_):
    """
    NaN typically indicates missing data, however several features use NaN to indicate
    a lack of a feature. This function determines if NaN indicates observation actually missing
    data by comparing related features.

    Ex: BsmtCond = NaN indicates no basement, but if related features possess data, we
    can assume that NaN actually represents missing data
    """

    # Feature groups
    bsmt_vars = ["BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2"]
    garage_vars = ["GarageType", "GarageFinish", "GarageQual", "GarageCond"]
    fireplace_vars = ["Fireplaces", "FireplaceQu"]
    pool_vars = ["PoolArea", "PoolQC"]

    dict_ = {
        "bsmt_vars": [bsmt_vars],
        "garage_vars": [garage_vars],
        "fireplace_vars": [fireplace_vars],
        "pool_vars": [pool_vars],
    }

    # Isolate feature columns
    # for var_list in dict_:
    for var_list in dict_:
        feats = dict_[var_list][0]  # feature group from dictionary
        var_df = dataframe_[feats]  # sub-df of related features
        string = "None"  # indicates observation lacks feature

        # id indices of rows containing 'None'
        idx = [
            i
            for i in range(0, var_df.shape[0])
            if ("None" in var_df.iloc[i].unique().tolist())
        ]

        # create sublist of indices containing any data other than 'None'
        if len(idx) > 0:
            if var_list == "fireplace_vars":
                # Any houses missing FireplaceQu, yet include # of fireplaces?
                sub_idx = [
                    sub_idx for sub_idx in idx if var_df.Fireplaces.iloc[sub_idx] != 0
                ]
            elif var_list == "pool_vars":
                # Any houses missing PoolQC, yet include pool sq. ft.?
                sub_idx = [sub_idx for sub_idx in idx if var_df.PoolArea.iloc[sub_idx] != 0]
            else:
                # If obv. missing basement, should expect 'None' across all bsmt features
                sub_idx = [
                    sub_idx
                    for sub_idx in idx
                    if len(var_df.iloc[sub_idx].unique().tolist()) > 1
                ]

            # Presents of sub_idx indicates actual missing data
            if len(sub_idx) > 0:
                var_df.iloc[sub_idx] = var_df.iloc[sub_idx].replace(string, np.nan)
                dataframe_[feats] = var_df

    print("Actual-missing values correctly identified")
    return dataframe_



# ========== Determine outliers of Continuous Data ==========
def outlier_idx(data, thresh):
    """
    Returns indices of observations that fall outside threshold sd
    """
    avg = data.mean()  # calc average
    stdev = data.std()  # calc standard deviation
    z_score = (data - avg) / stdev  # calc z_score
    ol = z_score > thresh  # boolean (True=outlier)
    outlier_idx = data.index[ol]  # indexes of outliers

    return list(outlier_idx)



# ========== Convert ordinal str features to numeric values ==========
def convert_ordinal_data(dataframe, ordinal_variables):
    '''
    Converts ordinal variables to numeric value based depending on values
    '''

    # Create dictionarys for mapping specific numeric values to ordinal values
    ord_replace_EXtoPO = {"Ex": 5, "Gd": 4, "TA": 3, "Fa": 2, "Po": 1}
    ord_replace_Fence = {"GdPrv": 4, "MnPrv": 3, "GdWo": 2, "MnWw": 1}
    ord_replace_BsmtFinType = {
        "GLQ": 6,
        "ALQ": 5,
        "BLQ": 4,
        "Rec": 3,
        "LwQ": 2,
        "Unf": 1,
    }
    ord_replace_BsmtExposure = {"Gd": 4, "Av": 3, "Mn": 2, "No": 1}
    ord_replace_GarageFinish = {"Fin": 4, "RFn": 3, "Unf": 2, "No": 0}
    ord_replace_PavedDrive = {"Y": 3, "P": 2, "N": 0}

    # Map and replace new values
    for feat in ordinal_variables:
        if feat == 'Fence':
            dataframe[feat] = dataframe[feat].map(ord_replace_Fence)
        elif feat == 'BsmtExposure':
            dataframe[feat] = dataframe[feat].map(ord_replace_BsmtExposure)
        elif feat == 'GarageFinish':
            dataframe[feat] = dataframe[feat].map(ord_replace_GarageFinish)
        elif feat == 'PavedDrive':
            dataframe[feat] = dataframe[feat].map(ord_replace_PavedDrive)
        elif feat in ['BsmtFinType1', 'BsmtFinType2']:
            dataframe[feat] = dataframe[feat].map(ord_replace_BsmtFinType)
        elif feat not in ['OverallCond', 'OverallQual']:
            dataframe[feat] = dataframe[feat].map(ord_replace_EXtoPO)
        # print("{0}: Unique values of {1}".format(feat, dataframe[feat].unique())
        dataframe[feat].fillna(0, inplace=True)  # addresses 'No Basement', 'No Pool', etc
        dataframe[feat].astype('float64') #standardize type

    print("Ordinal data conversion complete")
    return dataframe



# ========== Return summary of feature ==========
def explore_feat(dataframe, feat, sortby='count'):
    output = dataframe.groupby(feat)["SalePrice"].agg(["count", "mean"]).sort_values(
    ascending=False, by=sortby)
    return output



# ========== Transform Continuous and Ordinal Variables ==========
def transform_skewness(dataframe, continuous_variables, ordinal_variables):
    '''
    continous_variables = list of features
    ordinal_variables = list of features
    '''
    ord_skew_feat = ordinal_variables
    cont_skew_feat = continuous_variables

    store = []
    for var_type in ['ord_skew_feat', 'cont_skew_feat']:
        #Boxcox Transformation of Continuous Features
        for feat in cont_skew_feat:
            orig_skew = abs(stats.skew(dataframe[feat]))
            log_skew = abs(stats.skew( np.log1p(dataframe[feat]) ))

            if log_skew < orig_skew:
                dataframe[feat] = np.log1p(dataframe[feat])
                box_skew = abs(stats.skew( np.log1p(dataframe[feat]) ))

                if (log_skew > 0.75) & (box_skew < log_skew):
                    dataframe[feat] = np.log1p(dataframe[feat])
                    final_skew = box_skew
                    method = 'boxcox'
                else:
                    final_skew = log_skew
                    method = 'log'
            else:
                final_skew = orig_skew
                method = 'none'
            store.append(['continuous', feat, orig_skew, final_skew, method]) 
        #Power Transformation of Ordinal Features
        for feat in ord_skew_feat:
            orig_skew = abs(stats.skew( dataframe[feat] ))
            sqrt_skew = abs(stats.skew( np.power(dataframe[feat], .5) ))
            power_skew = abs(stats.skew( np.power(dataframe[feat], 2) ))
            cube_skew = abs(stats.skew( np.power(dataframe[feat], 3) ))

            if sqrt_skew < orig_skew:
                dataframe[feat] = np.power(dataframe[feat], .5)
                method = 'x^0.5'
            elif power_skew < orig_skew:
                if cube_skew < power_skew:
                    dataframe[feat] = np.power(dataframe[feat], 3)
                    method = 'x^3'
                else:
                    dataframe[feat] = np.power(dataframe[feat], 2) 
                    method = 'x^2'
            else:
                method='none'
            final_skew = abs(stats.skew( dataframe[feat] ))

            store.append(['ordinal', feat, orig_skew, final_skew, method]) 
                
    #Dataframe of stored skewness information
    store_df = pd.DataFrame(store, columns=['type','feature','orig_skewness','final_skewness', 'method']) 
    
    return dataframe, store_df 



# ========== Determine Outliers of Continuous Data ==========
def outlier_idx(data, thresh):

    avg = data.mean()  # calc average
    stdev = data.std()  # calc standard deviation
    z_score = (data - avg) / stdev  # calc z_score
    ol = z_score > thresh  # boolean (True=outlier)
    outlier_idx = data.index[ol].tolist()  # list of outliers

    return outlier_idx
