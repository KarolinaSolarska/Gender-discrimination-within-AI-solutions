# Gender-discrimination-within-AI-solutions
Based on the data which show that female faculty at the school were less likely to be full professors, more likely to be assistant professors, and earn less money than men, on average, a logistic regression model was built, explaining the probability of AI implementation based on the selected explanatory variables.

## Data
In folder 'data' you will find the data you need to run the code. 
You just need to open the file in RStudio and click run.
The code contains all the steps taken to create and select an appropriate econometric model. In addition, in the file you will find short eda, tests for the model, marginal effects, odds ratios. In folder you have also two codes, that were used to calculate marginal effects and linktest.

Unprepared data was downloaded from the OpenML website: https://www.openml.org/search?type=data&status=active&id=43626&sort=runs.

## AI Implementation - dependent variable
The binary variable AI Implementation determines whether the scientific literature about the implementation of the AI technology into the field of science exists and proves its reasonability (AI Implementation = 1). This variable is based on the Dept variable, which stands for Department. The initial variable was categorical with 6 categories, where 1=Biochemistry/Molecular Biology, 2=Physiology, 3=Genetics, 4=Pediatrics, 5=Medicine 6=Surgery. The transformation has been conducted based on the literature review, which gave us an outline, whether the specific department should be treated in the eyes of the business and job market as an achieved target for the improvements by AI implementation. AI Implementation is included in the model as explained variable, thus the variable Dept is excluded from the model. 

