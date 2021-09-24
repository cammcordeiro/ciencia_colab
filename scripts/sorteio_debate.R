############################
### Sorteio apresentacao ###
############################

set.seed(13)

#############
## 21-07-2021
# "Marina", "Carolina" -> não incluídas

# Sorteio de 3 = (1) apresentar, (2) e (3) debater
c("Vanessa", "Gustavo", "Pedro", "Isabella", "Carlos", "Gabriel", "Nilson") %>% 
  sample(3)

# [1] "Gustavo"  "Carlos"   "Isabella"
# - governança de dados de origem de conhecimento de povos indígenas 


#############
## 22-07-2021
# "Carolina" "Gustavo"  "Carlos"   "Isabella" -> não incluídos

# Sorteio de 3 = (1) apresentar, (2) e (3) debater
c("Vanessa", "Pedro", "Gabriel", "Nilson") %>% 
  sample(3)

# "Gabriel" "Pedro"   "Nilson"


#############
## 23-07-2021
# "Gustavo"  "Carlos"  "Isabella" "Gabriel" "Pedro"   "Nilson" -> não incluídos

# Ultimos 3 = (1) apresentar ("Vanessa"), (2) debater
c("Gustavo", "Pedro", "Carlos", "Nilson") %>% 
  sample(2)

"Carlos" "Nilson"

#############
## 24-07-2021

# Sem sorteio

c("Vanessa", "Gustavo", "Pedro", "Isabella", "Carlos", "Gabriel", "Nilson") %>% 
  sample(2)

# (1) apresentar ("Marina"), (2) debater




