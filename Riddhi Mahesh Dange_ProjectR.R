# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the dataset
country_data <- read.csv("/Users/riddhidange/Desktop/Academics/fall2023/FA550/Project/world-data-2023.csv")

# Display the first few rows of the dataset
head(country_data)

# Renaming the columns
colnames(country_data) <- c(
  "Country",
  "Population_Density",
  "Abbreviation",
  "Agricultural_Land_Percentage",
  "Land_Area_Km2",
  "Armed_Forces_Size",
  "Birth_Rate",
  "Calling_Code",
  "Capital_Major_City",
  "CO2_Emissions",
  "CPI",
  "CPI_Change",
  "Currency_Code",
  "Fertility_Rate",
  "Forested_Area_Percentage",
  "Gasoline_Price",
  "GDP",
  "Gross_Primary_Education_Enrollment",
  "Gross_Tertiary_Education_Enrollment",
  "Infant_Mortality",
  "Largest_City",
  "Life_Expectancy",
  "Maternal_Mortality_Ratio",
  "Minimum_Wage",
  "Official_Language",
  "Out_of_Pocket_Health_Expenditure",
  "Physicians_Per_Thousand",
  "Total_Population",
  "Labor_Force_Participation_Rate",
  "Tax_Revenue_Percentage_of_GDP",
  "Total_Tax_Rate",
  "Unemployment_Rate",
  "Urban_Population_Percentage",
  "Latitude",
  "Longitude"
)

# Remove rows with any NA values for all columns
country_data <- na.omit(country_data)

# Display column names
colnames(country_data)

# Question 1:
# Correlation between forested area percentage and GDP
correlation_plot <- ggplot(country_data, aes(x = Forested_Area_Percentage, y = GDP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Forested Area Percentage and GDP",
       x = "Forested Area Percentage",
       y = "GDP") 
correlation_plot
#ggsave("Correlation between forested area percentage and GDP.png",correlation_plot, width = 10, height = 6 )

# Question 2:
# Fertility rate impact on GDP and CPI
fertility_impact_plot <- ggplot(country_data, aes(x = Fertility_Rate, y = GDP)) +
  geom_point() +
  labs(title = "Fertility Rate Impact on GDP",
       x = "Fertility Rate",
       y = "GDP")
fertility_impact_plot
#ggsave("Fertility rate impact on GDP and CPI.png",fertility_impact_plot, width = 10, height = 6 )

fertility_cpi_plot <- ggplot(country_data, aes(x = Fertility_Rate, y = CPI)) +
  geom_point() +
  labs(title = "Fertility Rate Impact on CPI",
       x = "Fertility Rate",
       y = "CPI")
fertility_cpi_plot
#ggsave("Fertility Rate Impact on CPI.png",fertility_cpi_plot, width = 10, height = 6 )

# Boxplot for Fertility Rate
fertility_boxplot <- ggplot(country_data, aes(x = "Fertility Rate", y = Fertility_Rate)) +
  geom_boxplot() +
  labs(title = "Distribution of Fertility Rate",
       x = "Fertility Rate",
       y = "Fertility Rate")


fertility_boxplot
#ggsave("Distribution of Fertility Rate.png",fertility_boxplot, width = 10, height = 6 )

# Question 3:
# Relationship between CO2 emissions and life expectancy
co2_life_expectancy_plot <- ggplot(country_data, aes(x = CO2_Emissions, y = Life_Expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between CO2 Emissions and Life Expectancy",
       x = "CO2 Emissions",
       y = "Life Expectancy")
co2_life_expectancy_plot
#ggsave("Relationship between CO2 Emissions and Life Expectance.png",co2_life_expectancy_plot, width = 10, height = 6 )

# Investment in healthcare infrastructure for high CO2 emission countries (Bubble Chart)
healthcare_investment_plot <- ggplot(country_data, aes(x = CO2_Emissions, y = Out_of_Pocket_Health_Expenditure, size = Population_Density)) +
  geom_point(alpha = 0.7) +
  labs(title = "Investment in Healthcare Infrastructure for High CO2 Emission Countries",
       x = "CO2 Emissions",
       y = "Healthcare Investment",
       size = "Population Density") 


healthcare_investment_plot
#ggsave("Investment in Healthcare Infrastructure for High CO2 Emission Countries.png",healthcare_investment_plot, width = 10, height = 6 )

# Question 4:
# Healthcare expenditure impact on life expectancy
healthcare_expenditure_plot <- ggplot(country_data, aes(x = Out_of_Pocket_Health_Expenditure, y = Life_Expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Healthcare Expenditure Impact on Life Expectancy",
       x = "Total Healthcare Expenditure",
       y = "Life Expectancy")
healthcare_expenditure_plot
#ggsave("Healthcare Expenditure Impact on Life Expectancy.png",healthcare_expenditure_plot, width = 10, height = 6 )


# Correlation between out-of-pocket health expenditure and life expectancy (Line Chart)
out_of_pocket_plot <- ggplot(country_data, aes(x = Out_of_Pocket_Health_Expenditure, y = Life_Expectancy)) +
  geom_line() +
  labs(title = "Correlation between Out-of-Pocket Health Expenditure and Life Expectancy",
       x = "Out-of-Pocket Health Expenditure",
       y = "Life Expectancy")
out_of_pocket_plot
#ggsave("Correlation between Out-of-Pocket Health Expenditure and Life Expectancy.png",out_of_pocket_plot, width = 10, height = 6 )


scatter_healthcare_plot <- ggplot(country_data, aes(x = Out_of_Pocket_Health_Expenditure, y = Life_Expectancy, size = Out_of_Pocket_Health_Expenditure, color = Population_Density)) +
  geom_point(alpha = 0.7) +
  labs(title = "Impact of Access to Healthcare Services on Life Expectancy",
       x = "Out-of-Pocket Health Expenditure",
       y = "Life Expectancy",
       size = "Out-of-Pocket Health Expenditure",
       color = "Population Density")

scatter_healthcare_plot
#ggsave("Impact of Access to Healthcare Services on Life Expectancy.png",scatter_healthcare_plot, width = 10, height = 6 )


# Question 5:
# Relationship between education levels and economic growth
education_growth_plot <- ggplot(country_data, aes(x = Gross_Primary_Education_Enrollment, y = GDP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Primary Education Enrollment and GDP",
       x = "Primary Education Enrollment",
       y = "GDP")

education_growth_plot
#ggsave("Relationship between Primary Education Enrollment and GDP.png",education_growth_plot, width = 10, height = 6 )



# Question 6:
# Factors influencing the adoption of clean energy technologies (Bubble Chart)
clean_energy_factors_plot <- ggplot(country_data, aes(x = GDP, y = CO2_Emissions, size = Urban_Population_Percentage)) +
  geom_point(alpha = 0.7) +
  labs(title = "Factors Influencing Clean Energy Adoption",
       x = "GDP",
       y = "CO2 Emissions",
       size = "Urban Population Percentage")
clean_energy_factors_plot
#ggsave("Factors Influencing Clean Energy Adoption.png",clean_energy_factors_plot, width = 10, height = 6 )


# Stacked Bar Chart for factors influencing clean energy adoption and CO2 emissions reduction
clean_energy_factors_chart <- ggplot(country_data, aes(x = Country, fill = factor(Tax_Revenue_Percentage_of_GDP))) +
  geom_bar() +
  labs(title = "Factors Influencing Clean Energy Adoption and CO2 Emissions Reduction",
       x = "Country",
       fill = "Tax Revenue Percentage of GDP")

clean_energy_factors_chart
#ggsave("Factors Influencing Clean Energy Adoption and CO2 Emissions Reduction.png",clean_energy_factors_chart, width = 10, height = 6 )







