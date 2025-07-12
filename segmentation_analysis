# AI Data Center Customer Segmentation using LPA (Latent Profile Analysis)
# European Market Analysis - Marketing Analytics Project

# ============================================================================
# 0. PACKAGE SETUP
# ============================================================================

packages <- c("dplyr", "ggplot2", "tidyLPA", "viridis", "knitr", "gridExtra", "scales")
install.packages(setdiff(packages, rownames(installed.packages())))
library(dplyr)
library(ggplot2)
library(tidyLPA)
library(viridis)
library(knitr)
library(gridExtra)
library(scales)

set.seed(123) 

# ============================================================================
# 1. DATA GENERATION
# ============================================================================

generate_customers <- function(n = 300) {
  org_types <- sample(c("University", "Startup", "Enterprise", "Regulated"), 
                      n, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))
  data <- data.frame(
    customer_id = 1:n,
    org_type = org_types
  )
  for(i in 1:n) {
    type <- data$org_type[i]
    if(type == "University") {
      data$annual_budget[i] <- rnorm(1, 400000, 150000)      
      data$monthly_compute_hours[i] <- rnorm(1, 1500, 500)   
      data$compliance_score[i] <- rnorm(1, 2.0, 0.8)         
      data$ai_maturity[i] <- rnorm(1, 2.2, 0.6)              
    } else if(type == "Startup") {
      data$annual_budget[i] <- rnorm(1, 250000, 100000)      
      data$monthly_compute_hours[i] <- rnorm(1, 1200, 400)   
      data$compliance_score[i] <- rnorm(1, 1.8, 0.5)         
      data$ai_maturity[i] <- rnorm(1, 3.5, 0.7)              
    } else if(type == "Enterprise") {
      data$annual_budget[i] <- rnorm(1, 1500000, 500000)     
      data$monthly_compute_hours[i] <- rnorm(1, 3000, 800)   
      data$compliance_score[i] <- rnorm(1, 3.2, 0.8)         
      data$ai_maturity[i] <- rnorm(1, 3.0, 0.6)              
    } else { # Regulated
      data$annual_budget[i] <- rnorm(1, 2000000, 600000)     
      data$monthly_compute_hours[i] <- rnorm(1, 2500, 600)   
      data$compliance_score[i] <- rnorm(1, 4.5, 0.5)         
      data$ai_maturity[i] <- rnorm(1, 2.5, 0.7)              
    }
  }
  # Clean up values (no negatives/out-of-bounds)
  data$annual_budget <- pmax(50000, data$annual_budget)
  data$monthly_compute_hours <- pmax(200, data$monthly_compute_hours)
  data$compliance_score <- pmax(1, pmin(5, data$compliance_score))
  data$ai_maturity <- pmax(1, pmin(5, data$ai_maturity))
  return(data)
}
customers <- generate_customers(300)

cat("Total customers:", nrow(customers), "\n")
print(table(customers$org_type))
print(summary(customers[,c("annual_budget", "monthly_compute_hours", "compliance_score", "ai_maturity")]))


# ============================================================================
# 2. DATA PREPARATION
# ============================================================================

lpa_data <- customers %>%
  select(annual_budget, monthly_compute_hours, compliance_score, ai_maturity) %>%
  mutate(
    budget_scaled = scale(log(annual_budget))[,1],    
    compute_scaled = scale(monthly_compute_hours)[,1],
    compliance_scaled = scale(compliance_score)[,1],
    ai_maturity_scaled = scale(ai_maturity)[,1]
  ) %>%
  select(budget_scaled, compute_scaled, compliance_scaled, ai_maturity_scaled)


# ============================================================================
# 3. LATENT PROFILE ANALYSIS (LPA)
# ============================================================================

lpa_comparison <- lpa_data %>%
  estimate_profiles(n_profiles = 1:6) %>%
  compare_solutions(statistics = c("AIC", "BIC", "Entropy"))
print(lpa_comparison)

# Try both "equal" and "varying" variance/covariance structures for comparison
model1 <- lpa_data %>%
  estimate_profiles(n_profiles = 1:5, 
                    variances = "equal", 
                    covariances = "zero") %>%
  compare_solutions(statistics = c("AIC", "BIC", "Entropy"))
print(model1)

model2 <- lpa_data %>%
  estimate_profiles(n_profiles = 1:5,
                    variances = "varying",
                    covariances = "zero") %>%
  compare_solutions(statistics = c("AIC", "BIC", "Entropy"))
print(model2)

# Choose best model (here: 4-profile solution, varying variances)
best_lpa_model <- lpa_data %>%
  estimate_profiles(n_profiles = 4, 
                    variances = "varying", 
                    covariances = "zero")
print(best_lpa_model)

lpa_results <- get_data(best_lpa_model)
customers$profile <- lpa_results$Class
customers$probability <- apply(lpa_results[,paste0("CPROB", 1:4)], 1, max)
cat("Average membership probability:", round(mean(customers$probability), 3), "\n")

# ============================================================================
# 4. PROFILE ANALYSIS TABLE
# ============================================================================

profile_analysis <- customers %>%
  group_by(profile) %>%
  summarise(
    size = n(),
    size_pct = round(n()/nrow(customers)*100, 1),
    avg_budget = round(mean(annual_budget), 0),
    avg_compute = round(mean(monthly_compute_hours), 0),
    avg_compliance = round(mean(compliance_score), 2),
    avg_ai_maturity = round(mean(ai_maturity), 2),
    avg_certainty = round(mean(probability), 3),
    dominant_org_type = names(sort(table(org_type), decreasing = TRUE))[1],
    .groups = 'drop'
  )
kable(profile_analysis, caption = "LPA Customer Profiles - Key Characteristics")

# Assign profile names (for plots/analysis)
profile_names <- c(
  "Budget-Conscious Researchers",    
  "Agile Tech Innovators",          
  "Enterprise Powerhouses",         
  "Compliance-First Organizations"
)
customers$profile_name <- profile_names[customers$profile]

# Profile x org_type crosstab
crosstab <- table(customers$profile, customers$org_type)
cat("\nProfile vs Org Type Table:\n")
print(crosstab)
cat("\nProfile % composition within org type:\n")
print(round(prop.table(crosstab, margin = 1) * 100, 1))


# ============================================================================
# 5. PROFILE CHARACTERISTICS PLOT
# ============================================================================

profile_means <- get_estimates(best_lpa_model) %>%
  filter(Category == "Means") %>%
  mutate(
    Variable = case_when(
      Parameter == "budget_scaled" ~ "Budget",
      Parameter == "compute_scaled" ~ "Compute Hours", 
      Parameter == "compliance_scaled" ~ "Compliance",
      Parameter == "ai_maturity_scaled" ~ "AI Maturity"
    ),
    Profile_Name = profile_names[Class]
  )

p1 <- ggplot(profile_means, aes(x = Variable, y = Estimate, 
                                color = Profile_Name, group = Profile_Name)) +
  geom_line(linewidth = 1.5, alpha = 0.8) +    # linewidth for lines
  geom_point(size = 4) +
  labs(title = "Customer Profile Characteristics",
       subtitle = "Standardized means across key dimensions",
       x = "", y = "Standardized Score") +
  scale_color_viridis_d(name = "Customer Profile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(p1)

# ============================================================================
# 6. PROFILE CLUSTERS IN FEATURE SPACE (Scatter)
# ============================================================================

p2 <- ggplot(customers, aes(x = annual_budget/1000, y = monthly_compute_hours, 
                            color = profile_name, size = probability)) +
  geom_point(alpha = 0.7) +
  labs(title = "Customer Segmentation: Budget vs Compute Requirements",
       subtitle = "Point size indicates membership probability",
       x = "Annual Budget (€ Thousands)", 
       y = "Monthly Compute Hours") +
  scale_color_viridis_d(name = "Customer Profile") +
  scale_size_continuous(name = "Membership\nProbability", range = c(1, 4)) +
  scale_x_continuous(trans = "log10", labels = comma) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p2)

# ============================================================================
# 7. COMPOSITION OF PROFILES BY ORG TYPE (Stacked Bar)
# ============================================================================

composition_data <- customers %>%
  count(profile_name, org_type) %>%
  group_by(profile_name) %>%
  mutate(percentage = n/sum(n)*100)

p3 <- ggplot(composition_data, aes(x = profile_name, y = percentage, fill = org_type)) +
  geom_col(position = "stack") +
  labs(title = "Profile Composition by Organization Type",
       x = "Customer Profile", y = "Percentage") +
  scale_fill_viridis_d(name = "Organization Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(p3)

# ============================================================================
# 8. MARKETING STRATEGY RECOMMENDATIONS (Console Output)
# ============================================================================

marketing_strategies <- data.frame(
  profile = 1:4,
  profile_name = profile_names,
  target_description = c(
    "Universities and research institutions with limited budgets but high computational needs",
    "AI-native startups with high technical expertise but budget constraints",
    "Large enterprises with substantial budgets and consistent AI workloads", 
    "Regulated industries with strict compliance requirements and security needs"
  ),
  value_proposition = c(
    "Cost-effective access to cutting-edge AI infrastructure for research excellence",
    "Flexible, scalable AI computing that grows with your startup journey",
    "Enterprise-grade AI infrastructure with dedicated support",
    "Compliant, secure AI computing with full data sovereignty guarantees"
  ),
  pricing_strategy = c(
    "Academic discounts (40-60% off), shared resource pools, grant program partnerships",
    "Freemium tier, pay-as-you-grow pricing, startup incubator rates",
    "Enterprise volume contracts, reserved capacity pricing, consulting packages",
    "Premium compliance tier, dedicated environments, audit-ready infrastructure"
  ),
  marketing_channels = c(
    "Academic conferences, research journals, university partnerships, grant applications",
    "Tech meetups, accelerator programs, developer communities, social media",
    "Enterprise sales teams, industry conferences, thought leadership, case studies",
    "Compliance webinars, industry associations, regulatory consultants, white papers"
  ),
  key_features = c(
    "Batch processing optimization, shared GPU pools, educational tools, research credits",
    "Auto-scaling, developer APIs, monitoring dashboards, technical documentation",
    "Dedicated clusters, 24/7 support, custom configurations, integration services",
    "GDPR compliance, SOC2 certification, audit trails, encrypted storage, data residency"
  ),
  success_metrics = c(
    "Research publications using platform, academic partnerships, citation impact",
    "Customer growth rate, usage scaling, startup success stories, community engagement",
    "Contract value, utilization rates, customer satisfaction, expansion revenue",
    "Compliance certifications passed, audit success rate, data security incidents (zero)"
  ),
  stringsAsFactors = FALSE
)

for(i in 1:4) {
  profile_data <- customers[customers$profile == i, ]
  strategy <- marketing_strategies[i, ]
  cat(strrep("=", 60), "\n")
  cat("PROFILE", i, ":", strategy$profile_name, "\n")
  cat(strrep("=", 60), "\n")
  cat("Market Size:", nrow(profile_data), "customers (", 
      round(nrow(profile_data)/nrow(customers)*100, 1), "%)\n")
  cat("Avg Budget: €", format(mean(profile_data$annual_budget), big.mark = ","), "\n")
  cat("Avg Compute: ", format(mean(profile_data$monthly_compute_hours), big.mark = ","), " hours/month\n")
  cat("Primary Org Type:", names(sort(table(profile_data$org_type), decreasing = TRUE))[1], "\n")
  cat("Membership Certainty:", round(mean(profile_data$probability), 3), "\n\n")
  cat("TARGET DESCRIPTION:\n", strategy$target_description, "\n\n")
  cat("VALUE PROPOSITION:\n", strategy$value_proposition, "\n\n")
  cat("PRICING STRATEGY:\n", strategy$pricing_strategy, "\n\n")
  cat("MARKETING CHANNELS:\n", strategy$marketing_channels, "\n\n")
  cat("KEY FEATURES:\n", strategy$key_features, "\n\n")
  cat("SUCCESS METRICS:\n", strategy$success_metrics, "\n")
}

# ============================================================================
# 9. REVENUE ANALYSIS & PLOT
# ============================================================================

revenue_analysis <- customers %>%
  group_by(profile, profile_name) %>%
  summarise(
    customers = n(),
    total_monthly_hours = sum(monthly_compute_hours),
    avg_budget = mean(annual_budget),
    .groups = 'drop'
  ) %>%
  mutate(
    price_per_hour = c(2.0, 2.5, 4.5, 6.0),          # Pricing per profile (update if needed)
    utilization_rate = c(0.6, 0.75, 0.9, 0.85),       # Utilization rate per profile
    monthly_revenue = total_monthly_hours * price_per_hour * utilization_rate,
    annual_revenue = monthly_revenue * 12,
    customer_lifetime_value = annual_revenue / customers
  )
kable(revenue_analysis, caption = "Revenue Potential by Customer Profile", digits = 0)

total_annual_revenue <- sum(revenue_analysis$annual_revenue)
cat("\nTotal Annual Revenue Potential: €", format(total_annual_revenue, big.mark = ","), "\n")

p5 <- ggplot(revenue_analysis, aes(x = reorder(profile_name, -annual_revenue), 
                                   y = annual_revenue/1000000, fill = factor(profile))) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0("€", round(annual_revenue/1000000, 1), "M")), 
            vjust = -0.5, fontface = "bold") +
  labs(title = "Annual Revenue Potential by Customer Profile",
       x = "Customer Profile", y = "Revenue (€ Millions)") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
print(p5)

# ============================================================================
# ALL PLOTS IN ONE GRID (optional)
# ============================================================================
# If you want to display all plots together:
# grid.arrange(p1, p2, p3, p5, nrow = 2)

cat("\n--- Detailed Strategic Recommendations by Segment ---\n")
cat("1. Universities:\n")
cat("   - Develop educational partnerships and host AI research seminars.\n")
cat("   - Provide tailored student/researcher pricing and cloud grants.\n")
cat("   - Offer workshops to raise AI maturity and adoption.\n\n")

cat("2. Startups:\n")
cat("   - Launch startup accelerator programs (credits, mentorship).\n")
cat("   - Flexible, usage-based billing and onboarding support.\n")
cat("   - Promote co-marketing opportunities for AI innovation.\n\n")

cat("3. Enterprises:\n")
cat("   - Sell premium service tiers with enhanced SLA and compliance features.\n")
cat("   - Co-develop industry-specific AI solutions and provide white-glove onboarding.\n")
cat("   - Assign dedicated Customer Success teams for upselling.\n\n")

cat("4. Regulated Organizations:\n")
cat("   - Emphasize compliance certifications and data protection.\n")
cat("   - Offer audit-ready reporting and consulting for regulations.\n")
cat("   - Invite to closed user groups to guide roadmap and policy.\n\n")

cat("--- Cross-Segment Next Steps ---\n")
cat("- Design targeted marketing messages for each segment.\n")
cat("- Monitor segment movement (change in AI maturity, spend) quarterly.\n")
cat("- Implement a feedback loop for continuous improvement.\n")
cat("- Use cluster assignments for personalization in the data center portal.\n")
cat("- Analyze cluster profitability and lifetime value for resource allocation.\n\n")

cat("--- Advanced Analytics & Business Actions ---\n")
cat("- Track customer churn and upsell rates by segment.\n")
cat("- Use clustering as input for recommendation systems (personalized offers).\n")
cat("- Periodically retrain clusters with new data to adapt to market trends.\n")
cat("- Run A/B tests on segment-specific campaigns.\n")
cat("- Integrate findings with CRM for sales team enablement.\n")

