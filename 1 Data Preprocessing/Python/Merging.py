import pandas as pd

# sources
pathOrders = '../../0 Data/order_data_cleaned_P.csv'
pathClicks = '../../0 Data/clickstream_data_cleaned_P.csv'

# results
pathMerge = "../../0 Data/merged_P.csv"
pathCustomers = "../../0 Data/merged_Customers_P.csv"

#############################################################################

orderColumns = ["City",
                        "Country",
                        "US.State",
                        "Age",
                        "Marital.Status",
                        "Gender",
                        "Audience",
                        "Truck.Owner",
                        "RV.Owner",
                        "Motorcycle.Owner",
                        "Working.Woman",
                        "Presence.Of.Children",
                        "Speciality.Store.Retail",
                        "Oil.Retail.Activity",
                        "Bank.Retail.Activity",
                        "Finance.Retail.Activity",
                        "Miscellaneous.Retail.Activity",
                        "Upscale.Retail",
                        "Upscale.Speciality.Retail",
                        "Retail.Activity"
                        ]

clickColumns = ["WhichDoYouWearMostFrequent",
                        "YourFavoriteLegcareBrand",
                        "Registration.Gender",
                        "NumberOfChildren",
                        "DoYouPurchaseForOthers",
                        "HowDoYouDressForWork",
                        "HowManyPairsDoYouPurchase",
                        "YourFavoriteLegwearBrand",
                        "WhoMakesPurchasesForYou",
                        "NumberOfAdults",
                        "HowDidYouHearAboutUs",
                        "SendEmail",
                        "HowOftenDoYouPurchase",
                        "HowDidYouFindUs",
                        "City",
                        "US.State",
                        "Year.of.Birth",
                        "Email",
                        "Truck.Owner",
                        "RV.Owner",
                        "Motorcycle.Owner",
                        "Value.Of.All.Vehicles",
                        "Age",
                        "Other.Indiv...Age",
                        "Marital.Status",
                        "Working.Woman",
                        "Mail.Responder",
                        "Bank.Card.Holder",
                        "Gas.Card.Holder",
                        "Upscale.Card.Holder",
                        "Unknown.Card.Type",
                        "TE.Card.Holder",
                        "Premium.Card.Holder",
                        "Presence.Of.Children",
                        "Number.Of.Adults",
                        "Estimated.Income.Code",
                        "Home.Market.Value",
                        "New.Car.Buyer",
                        "Vehicle.Lifestyle",
                        "Property.Type",
                        "Loan.To.Value.Percent",
                        "Presence.Of.Pool",
                        "Year.House.Was.Built",
                        "Own.Or.Rent.Home",
                        "Length.Of.Residence",
                        "Mail.Order.Buyer",
                        "Year.Home.Was.Bought",
                        "Home.Purchase.Date",
                        "Number.Of.Vehicles",
                        "DMA.No.Mail.Solicitation.Flag",
                        "DMA.No.Phone.Solicitation.Flag",
                        "CRA.Income.Classification",
                        "New.Bank.Card",
                        "Number.Of.Credit.Lines",
                        "Speciality.Store.Retail",
                        "Oil.Retail.Activity",
                        "Bank.Retail.Activity",
                        "Finance.Retail.Activity",
                        "Miscellaneous.Retail.Activity",
                        "Upscale.Retail",
                        "Upscale.Speciality.Retail",
                        "Retail.Activity",
                        "Dwelling.Size",
                        "Dataquick.Market.Code",
                        "Lendable.Home.Equity",
                        "Home.Size.Range",
                        "Lot.Size.Range",
                        "Insurance.Expiry.Month",
                        "Dwelling.Unit.Size",
                        "Month.Home.Was.Bought",
                        "Available.Home.Equity",
                        "Minority.Census.Tract",
                        "Year.Of.Structure",
                        "Gender",
                        "Occupation",
                        "Other.Indiv...Gender",
                        "Other.Indiv...Occupation"
]

#############################################################################
# normal merge

# read data
clicks = pd.read_csv(pathClicks, sep=",", dtype=str)
orders = pd.read_csv(pathOrders, sep=",", dtype=str)

# merging
mergedData = pd.merge(clicks, orders, how='inner', left_on=['Customer ID'], right_on=['Customer ID'])

# save data
print(list(mergedData.shape))
mergedData.to_csv(path_or_buf=pathMerge, index=False)

#############################################################################
# customer data merge



# build subset on Customer Columns
clickCustomer1 = [w.replace('.', ' ') for w in clickColumns]
clickCustomer1 = [w.replace('   ', '\. ') for w in clickCustomer1]
clickCustomer1.append('Customer ID')
clicks = clicks[clickCustomer1]
clicks = clicks.drop_duplicates(subset=['Customer ID'])

orderCustomer1 = [w.replace('.', ' ') for w in orderColumns]
orderCustomer1.append('Customer ID')
orders = (orders[orderCustomer1])
orders = orders.drop_duplicates(subset=['Customer ID'])

# merging
mergedCustomer = pd.merge(clicks, orders, how='inner', left_on=['Customer ID'], right_on=['Customer ID'], suffixes=('', '_y'))
mergedCustomer.drop(list(mergedCustomer.filter(regex='_y$')), axis=1, inplace=True) #delete duplicates
print(list(mergedCustomer.shape))
mergedCustomer.to_csv(path_or_buf=pathCustomers, index=False)

