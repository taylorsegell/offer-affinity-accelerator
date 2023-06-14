"""
Sample Materials, provided under license.
Licensed Materials - Property of IBM
© Copyright IBM Corp. 2019. All Rights Reserved.
US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
"""

import pandas as pd
import numpy as np
import datetime
import sys
from dateutil.relativedelta import relativedelta
import os
import json

class OfferAffinityPrep():

    def __init__(self, train_or_score, product_id, effective_date_earliest,
                 effective_date_latest, nulls_threshold,
                 max_num_cat_cardinality, customer_id_col,
                 customer_effective_date_col, customer_relationship_start_date_col,
                 customer_summary_end_date, customer_product_summary_end_date_col,
                 required_product_attributes, default_attributes, scoring_date='2018-09-30'):

        self.train_or_score = train_or_score
        self.product_id = product_id
        self.effective_date_earliest = effective_date_earliest
        self.effective_date_latest = effective_date_latest
        self.scoring_date = scoring_date
        self.nulls_threshold = nulls_threshold
        self.max_num_cat_cardinality = max_num_cat_cardinality
        self.customer_id_col = customer_id_col
        self.customer_effective_date_col = customer_effective_date_col
        self.customer_relationship_start_date_col = customer_relationship_start_date_col
        self.customer_summary_end_date = customer_summary_end_date
        self.customer_product_summary_end_date_col = customer_product_summary_end_date_col
        self.required_product_attributes = required_product_attributes
        self.default_attributes = default_attributes

        self.effective_date_latest = datetime.datetime.strptime(effective_date_latest, '%Y-%m-%d')
        self.effective_date_earliest = datetime.datetime.strptime(effective_date_earliest, '%Y-%m-%d')
        self.scoring_date = datetime.datetime.strptime(scoring_date, '%Y-%m-%d')

        self.product_list = list(product_id)

        if self.train_or_score == 'train':
            # create a dictionary with all values for user inputs. We will
            # save this out and use it for scoring
            # to ensure that the user inputs are consistent across train and
            # score notebooks
            # exclude variables that won't be used for scoring
            self.training_metadata_dict = {'product_list': self.product_list,
                                'nulls_threshold': nulls_threshold,
                                'max_num_cat_cardinality': max_num_cat_cardinality,
                                'effective_date_earliest': effective_date_earliest,
                                'effective_date_latest': effective_date_latest,
                                'customer_id_col': customer_id_col,
                                'customer_effective_date_col': customer_effective_date_col,
                                'customer_relationship_start_date_col': customer_relationship_start_date_col,
                                'customer_summary_end_date': customer_summary_end_date,
                                'customer_product_summary_end_date_col': customer_product_summary_end_date_col,
                                'required_product_attributes': required_product_attributes,
                                'default_attributes': default_attributes}

    # this function filters the dataframe to only include the columns that are
    # specified by the user in the training notebook
    def filter_attributes(self, df, columns_required, default_attributes):

        # the attributes we will use are the required ones plus those
        # specified in default_attributes
        working_attributes = columns_required + default_attributes
        # check to make sure we don't have duplicate columns names
        working_attributes = list(set(working_attributes))
        # check to make sure that the attributes are in the original dataframe
        if set(working_attributes) - set(df.columns) == 0:
            print('Invalid column names, no column names in columns_required or default_attributes lists are contained in the dataframe')

        # check to see if any columns passed in the list are not actually in the dataframe, print them to screen
        # and remove from the list of working_attributes
        cols_passed_but_not_in_df = [attribute for attribute in working_attributes if attribute not in df.columns]
        if len(cols_passed_but_not_in_df) > 0:
            print(str(len(cols_passed_but_not_in_df)) + ' columns were passed but are not contained in the data. :' + str(cols_passed_but_not_in_df))
            working_attributes = [col for col in working_attributes if col not in cols_passed_but_not_in_df]

        df = df[working_attributes]
        return df

    # this function filters to include only data between the effective dates
    # for training, this filters to dates greater than the earliest effective date and less than the latest effective date
    def filter_timewindow(self, df, mode):
        if mode == 'train':
            if self.effective_date_earliest >= self.effective_date_latest:
                print('Invalid Effective Date Earliest/Latest combination. Please adjust the parameters', file=sys.stderr)
                return None

            # for training, filter to only include data between the specified effective dates
            df = df[(df[self.customer_effective_date_col] >= self.effective_date_earliest) & (df[self.customer_effective_date_col] <= self.effective_date_latest)]

            # keep only the most recent product offered record for each customer
            df = df.sort_values(by=[self.customer_id_col, self.customer_product_summary_end_date_col], ascending=[True, False])
            df = df.groupby(self.customer_id_col).first().reset_index()
            
        elif mode == 'score':
            # for scoring, filter to only include data before the specified scoring date
            df = df[df[self.customer_summary_end_date] <= self.scoring_date]
            
            # if scoring, sort by the customer id and summary end date - user specifies the scoring date, we take the latest date of data - should usually be the month specified
            df = df.sort_values(by=[self.customer_id_col, self.customer_summary_end_date], ascending=[True, False])
            df = df.groupby(self.customer_id_col).first().reset_index()
          

        # if all records have been filtered out print the message and return None
        if df.shape[0] == 0:
            print('Specified effective dates filtered out all data', file=sys.stderr)
            return None

        return df

    # This function does some data cleaning by removing columns that have constant or missing values
    # All numeric data that has only 1 value is removed (constants)
    # For categorical variables, we drop columns that have only 1 unique value (constants)
    # For categoricals, we drop columns that have a cardinality greater than or equal to max_num_cat_cardinality
    # We drop columns that have null values above the specified threshold, nulls_threshold
    def drop_dataframe_columns(self, df, keep_cols, nulls_threshold, max_num_cat_cardinality):

        print('Before cleaning, we had ' + str(df.shape[1]) + ' columns.')
        # get the numeric columns
        numeric_cols = list(df.select_dtypes(include=[np.number]).columns)
        # remove the columns that are required from the list
        numeric_cols = list(set(numeric_cols) - set(keep_cols))

        # drop all numeric columns that just contain a constant value, min=max
        # record cols that we are dropping and remove after iterating over the list
        cols_to_remove = []
        for col in numeric_cols:
            curr_col = df[col]
            if curr_col.max() == curr_col.min():
                df.drop(col, axis=1, inplace=True)
                # remove the column from our list of numerical variables
                cols_to_remove.append(col)

        numeric_cols = list(set(numeric_cols) - set(cols_to_remove))

        # get a count of number of null values in each column,
        # if the number of nulls is greater than a threshold percentage, drop the column
        cols_to_remove = []
        for col in numeric_cols:
            curr_col = df[col]
            if (curr_col.isna().sum() / curr_col.shape[0]) > nulls_threshold:
                df.drop(col, axis=1, inplace=True)
                # add the column name to the list of attributes to remove
                cols_to_remove.append(col)

        numeric_cols = list(set(numeric_cols) - set(cols_to_remove))

        # get the string and datetime columns
        string_cols = list(df.select_dtypes(include=[object]).columns)
        # remove the columns that are required from the list
        string_cols = list(set(string_cols) - set(keep_cols))

        datetime_cols = list(df.select_dtypes(include=[np.datetime64]).columns)
        # remove the columns that are required from the list
        datetime_cols = list(set(datetime_cols) - set(keep_cols))

        # treat string and datetime cols the same for below
        not_num_cols = string_cols + datetime_cols

        # remove string columns that have nulls greater than the threshold
        cols_to_remove = []
        for col in not_num_cols:
            curr_col = df[col]
            if (curr_col.isna().sum() / curr_col.shape[0]) > nulls_threshold:
                df.drop(col, axis=1, inplace=True)
                # add the column name to the list of tho
                cols_to_remove.append(col)

        string_cols = list(set(string_cols) - set(cols_to_remove))

        # drop categorical variables that are constant or more than max_num_cat_cardinality categories
        for col in string_cols:
            col_cardinality = df[col].nunique()
            if col_cardinality == 1 or col_cardinality >= max_num_cat_cardinality:
                df.drop(col, axis=1, inplace=True)

        print('After cleaning, we have ' + str(df.shape[1]) + ' columns.')

        return df

    # This function takes a dataframe, a list of columns and a multiplier
    # and replaces values that are more than multiplier * standard deviations from the mean
    # function is not used by default, change handle_and_clean_outliers in handle_missing_vals_and_categoricals to True to enable
    def clean_outliers(self, df, column_list, multiplier=5):
        for col in column_list:
            col_std = df[col].std()
            col_mean = df[col].mean()
            df.loc[df[col] >= col_mean + (multiplier * col_std), col] = col_mean + (multiplier * col_std)

        return df

    # function to get the difference between 2 dates returned in months
    def udf_n_months(self, date1, date2):
        month_dif = (relativedelta(date1, date2).months +
                     relativedelta(date1, date2).years * 12)
        return month_dif

    # This function calls additional functions for cleaning, it fills in missing data, creates a variable for customer tenure and dummies for categorical variables
    # if scoring, the function has an additional step of getting the scoring dataframe into the same order with same columns as the training dataset
    # if the mode is training, the function returns the dataframe and a dictionary with the mean values used for each column to fill in missing data
    # if in scoring mode, the function returns just the dataframe
    def handle_missing_vals_and_categoricals(self, df, train_or_score, product_id, columns_required):

        numeric_cols = list(df.select_dtypes(include=[np.number]).columns)
        numeric_cols = list(set(numeric_cols) - set(columns_required))
        string_cols = list(df.select_dtypes(include=[object]).columns)

        self.handle_and_clean_outliers = False
        self.std_multiplier = 5

        if train_or_score == 'train':
            if self.handle_and_clean_outliers:
                df = self.clean_outliers(df, numeric_cols, self.std_multiplier)

        # for string columns replace nulls with 'Unknown'
        # for numerical replace with mean. If there are no values for the column to calculate a mean (can happen in scoring), fill with 0 instead
        for col in string_cols:
            df[col].fillna('Unknown', inplace=True)

        if train_or_score == 'train':
            col_missing_data_means_dict = {}
            for col in numeric_cols:
                col_mean = df[col].mean()
                # if the whole column is null (can happen when scoring, esp if just 1 customer), fill the value with 0
                if pd.isnull(col_mean):
                    df[col].fillna(0, inplace=True)
                    col_missing_data_means_dict[col] = 0
                else:
                    df[col].fillna(col_mean, inplace=True)
                    col_missing_data_means_dict[col] = col_mean

        #elif train_or_score == 'score':
            #training_means_and_cols_dict = joblib.load(open(self.project_path + '/datasets/training_means_and_cols.joblib', 'rb'))

            #for col in numeric_cols:
                # only update means if the product is in the list of training columns
                #if col in training_means_and_cols_dict[product_id]['training_cols']:
                    #col_mean = training_means_and_cols_dict[product_id]['col_means'][col]
                    #df[col].fillna(col_mean, inplace=True)

        if train_or_score == 'train':
            if self.customer_relationship_start_date_col in df.columns:
                df = df[df[self.customer_relationship_start_date_col] <= self.effective_date_latest]
                if df.shape[0] == 0:
                    print('Error: No data to train with, relationship start date is before the latest effective date', file=sys.stderr)
                else:
                    print('Adding a column for customer tenure')
                    df['CUSTOMER_TENURE_IN_MONTHS'] = df.apply(lambda x: self.udf_n_months(self.effective_date_latest, x[self.customer_relationship_start_date_col]), axis=1)
        elif train_or_score == 'score':
            if self.customer_relationship_start_date_col in df.columns:
                df = df[df[self.customer_relationship_start_date_col] <= self.scoring_date]
                if df.shape[0] == 0:
                    print('Error: No data to train with, relationship start date is before the effective date', file=sys.stderr)
                else:
                    print('Adding a column for customer tenure')
                    df['CUSTOMER_TENURE_IN_MONTHS'] = df.apply(lambda x: self.udf_n_months(self.scoring_date, x[self.customer_relationship_start_date_col]), axis=1)
        else:
            print('Specify train or score in train_or_score variable')

        # Create dummy variables for categorical features and drop original
        print('Creating dummies for categorical variables')
        for col in string_cols:
            df = pd.concat([df, pd.get_dummies(df[col], prefix=col)], axis=1)
            df.drop(col, axis=1, inplace=True)

        # remove date type columns
        datetime_cols = list(df.select_dtypes(include=[np.datetime64]).columns)
        df.drop(datetime_cols, axis=1, inplace=True)

        if train_or_score == 'train':
            return df, col_missing_data_means_dict

        if train_or_score == 'score':
            return df

    # This is the main function to prep the data
    # It loops through each product id specified and calls the functions to prep the data
    # The function returns a dictionary with a dataset for each product id that can be used for building the models
    def prep_data(self, df_raw, train_or_score):
 
        columns_required = [self.customer_id_col, self.customer_effective_date_col, self.customer_relationship_start_date_col, self.customer_summary_end_date]

        # clean the list of product ids and add the relevany columns to the required columns list
        # each product will have a column in format of pid_ product id, product_summary and ending in each string in required_product_attributes list
        for p_id in self.product_list:
            # clean the product name so that it's the same format that's used in column names
            p_id = str(p_id).upper()
            # spaces to underscores
            p_id = p_id.replace(' ', '_')
            # remove unwanted characters
            to_remove = ",;{}()="
            for char in to_remove:
                p_id = p_id.replace(char, '')

            # get the column names - which consist of product, original table name (customer_product_summary) and the column names from customer_product_summary table
            for col in self.required_product_attributes:
                new_var_name = 'PID_' + p_id + '_CUSTOMER_PRODUCT_SUMMARY_' + col

                # update the columns_required list to include the required attributes per product - eg. PID_EDUCATION_CUSTOMER_PRODUCT_SUMMARY_PRODUCT_OFFERED_INDICATOR
                columns_required.append(new_var_name)

        # filter to only include data within the time frames specified
        df_prep = self.filter_timewindow(df_raw, train_or_score)
        if df_prep is None:
            return None

        # filter to only include variables that are specified - required and default list
        df_prep = self.filter_attributes(df_prep, columns_required, self.default_attributes)
        if df_prep is None:
            return None
        # loop through each product id
        # for each id, filter to only inlcude customers who were contacted about the product
        # clean data by removing columns with lot of missing values
        # fill in remaining missing values
        # when training, store the values used for calculating the missing values and also the columns used in the training dataset

        result_map = {}
        
        # dictionaries to store the calculated values when training and column names in training dataset
        # can be applied at scoring time
        if train_or_score == 'train':
            self.training_metadata_dict['col_means'] = {}
            self.training_metadata_dict['cols_used_for_training'] = {}

        for p_id in self.product_list:
            print('Prepping for ' + str(p_id))
            # clean the product name so that it's the same format that's used in column names
            p_id = str(p_id).upper()
            # spaces to underscores
            p_id = p_id.replace(' ', '_')
            # remove unwanted characters
            to_remove = ",;{}()="
            for char in to_remove:
                p_id = p_id.replace(char, '')

            purchased_col = "PID_" + p_id + '_CUSTOMER_PRODUCT_SUMMARY_PRODUCT_OWNED_INDICATOR'
            contacted_col = "PID_" + p_id + '_CUSTOMER_PRODUCT_SUMMARY_PRODUCT_OFFERED_INDICATOR'

            if train_or_score == 'train':

                # assumption that customers who were contacted were randomly choosen
                df_prepped = df_prep[df_prep[contacted_col] == 1].copy()
                if df_prepped.shape[0] == 0:
                    print('No data to train with, nobody was contacted', file=sys.stderr)
                    return None

                # we only do this for training, as when scoring, we already know the columns dropped from training
                if train_or_score == 'train':
                    df_prepped = self.drop_dataframe_columns(df_prepped, columns_required, self.nulls_threshold, self.max_num_cat_cardinality)

                df_prepped, self.training_metadata_dict['col_means'][p_id] = self.handle_missing_vals_and_categoricals(df_prepped, train_or_score, p_id, columns_required)

                df_prepped.drop(contacted_col, axis=1, inplace=True)
                df_prepped.rename(columns={purchased_col: 'Target'}, inplace=True)

                # drop the columns that end in PRODUCT_OWNED_INDICATOR or PRODUCT_OFFERED_INDICATOR
                df_prepped = df_prepped[df_prepped.columns[~df_prepped.columns.str.endswith('_CUSTOMER_PRODUCT_SUMMARY_PRODUCT_OWNED_INDICATOR')]]
                df_prepped = df_prepped[df_prepped.columns[~df_prepped.columns.str.endswith('_CUSTOMER_PRODUCT_SUMMARY_PRODUCT_OFFERED_INDICATOR')]]

                # drop customer_id column
                df_prepped.drop(self.customer_id_col, axis=1, inplace=True)

                result_map[p_id] = df_prepped

                temp_col_list = list(result_map[p_id].columns)
                temp_col_list.remove('Target')
                self.training_metadata_dict['cols_used_for_training'][p_id] = temp_col_list

            elif train_or_score == 'score':

                df_prepped = self.handle_missing_vals_and_categoricals(df_prep, train_or_score, p_id, columns_required)

                if df_prepped.shape[0] == 0:
                    print('No data to score', file=sys.stderr)
                    return None

                # drop the columns that end in PRODUCT_OWNED_INDICATOR or PRODUCT_OFFERED_INDICATOR
                df_prepped = df_prepped[df_prepped.columns[~df_prepped.columns.str.endswith('_CUSTOMER_PRODUCT_SUMMARY_PRODUCT_OWNED_INDICATOR')]]
                df_prepped = df_prepped[df_prepped.columns[~df_prepped.columns.str.endswith('_CUSTOMER_PRODUCT_SUMMARY_PRODUCT_OFFERED_INDICATOR')]]

                result_map[p_id] = df_prepped

            print('Final dataset shape: ' + str(df_prepped.shape))

        # add the user inputs to the dictionary for saving out
        if train_or_score == 'train':
            # save the user inputs and the columns used for building models 
            with open('/project_data/data_asset/training_user_inputs_and_prepped_column_names_and_means.json', 'w') as f:
                json.dump(self.training_metadata_dict, f)

        return result_map
