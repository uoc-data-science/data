library(tidyverse)

# sources
pathOrders = '0 Data/order_data_cleaned_R.csv'
pathClicks = '0 Data/clickstream_data_cleaned_R.csv'

# results
pathCustomers = "0 Data/merged_Customers_R.csv"

#############################################################################

orderColumns = c("City",
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
)

clickColumns = c("WhichDoYouWearMostFrequent",
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
)
#append Customer ID
orderColumns <- c(orderColumns, "Customer.ID")
clickColumns <- c(clickColumns, "Customer.ID")

#read data
orders <- read.csv(file=pathOrders)
clicks <- read.csv(file=pathClicks)

#select subsets
orderCustomer1 <- subset(orders, select=orderColumns)
clicksCustomer1 <- subset(clicks, select=clickColumns)

#remove duplicate rows based on CustomerID
orderCustomer1 <- orderCustomer1[!duplicated(orderCustomer1$Customer.ID),]
clicksCustomer1 <- clicksCustomer1[!duplicated(clicksCustomer1$Customer.ID),]

#merging
mergedCustomers <- merge(clicksCustomer1, orderCustomer1, by.clicksCustomer1="Customer.ID")
mergedCustomers <- arrange(mergedCustomers, Customer.ID)

write.table(mergedCustomers, file = pathCustomers, sep=",", row.names=FALSE)
