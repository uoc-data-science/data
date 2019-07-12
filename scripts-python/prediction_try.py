import read_data
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
# Columns:


["Age", "Available_Home_Equity", "Bank_Card_Holder", "Bank_Retail_Activity", "City", "Dwelling_Size",
     "Dwelling_Unit_Size", "Email", "Estimated_Income_Code", "Finance_Retail_Activity", "Gas_Card_Holder",
     "Gender", "Home_Market_Value", "Home_Purchase_Date", "Household_Status", "HowDidYouHearAboutUs",
     "HowDoYouDressForWork", "HowManyPairsDoYouPurchase", "HowOftenDoYouPurchase", "Marital_Status",
     "Minority_Census_Tract", "Miscellaneous_Retail_Activity", "Motorcycle_Owner", "New_Bank_Card",
     "New_Car_Buyer", "Number_Of_Adults", "Number_Of_Credit_Lines", "Number_Of_Vehicles", "Oil_Retail_Activity",
     "Own_Or_Rent_Home", "Premium_Card_Holder", "Presence_Of_Children", "Presence_Of_Pool", "Property_Type",
     "RV_Owner", "Retail_Activity", "SendEmail", "Speciality_Store_Retail", "TE_Card_Holder", "Truck_Owner",
     "US_State", "Unknown_Card_Type", "Upscale_Card_Holder", "Upscale_Retail", "Upscale_Speciality_Retail",
     "Value_Of_All_Vehicles", "Vehicle_Lifestyle", "WhichDoYouWearMostFrequent", "Working_Woman",
     "YourFavoriteLegcareBrand", "YourFavoriteLegwearBrand", "Product_Family_ID"]


X_train, X_test, y_train, y_test = train_test_split(order[["Age", "Available_Home_Equity", "Bank_Card_Holder", "Bank_Retail_Activity", "City", "Dwelling_Size",
     "Dwelling_Unit_Size", "Email", "Estimated_Income_Code", "Finance_Retail_Activity", "Gas_Card_Holder",
     "Gender", "Home_Market_Value", "Home_Purchase_Date", "Household_Status", "HowDidYouHearAboutUs",
     "HowDoYouDressForWork", "HowManyPairsDoYouPurchase", "HowOftenDoYouPurchase", "Marital_Status",
     "Minority_Census_Tract", "Miscellaneous_Retail_Activity", "Motorcycle_Owner", "New_Bank_Card",
     "New_Car_Buyer", "Number_Of_Adults", "Number_Of_Credit_Lines", "Number_Of_Vehicles", "Oil_Retail_Activity",
     "Own_Or_Rent_Home", "Premium_Card_Holder", "Presence_Of_Children", "Presence_Of_Pool", "Property_Type",
     "RV_Owner", "Retail_Activity", "SendEmail", "Speciality_Store_Retail", "TE_Card_Holder", "Truck_Owner",
     "US_State", "Unknown_Card_Type", "Upscale_Card_Holder", "Upscale_Retail", "Upscale_Speciality_Retail",
     "Value_Of_All_Vehicles", "Vehicle_Lifestyle", "WhichDoYouWearMostFrequent", "Working_Woman",
     "YourFavoriteLegcareBrand", "YourFavoriteLegwearBrand"]], order[["Product_Family_ID"]], test_size=0.3, random_state=42)

