# TP01-DataMining
# **Description du problème**

Vous êtes engagé par le directeur des ressources humaines d'une entreprise pour enquêter sur les raisons pour lesquelles les employés démissionnent. Il vous a fourni une base de données sur les employée de l’année précédente. Il veut comprendre quels sont les indicateurs les plus pertinents pour savoir qui envisage de quitter.

Vous devez travailler avec l'ensemble d’apprentissage (train.csv) pour analyser les données et
entraîner des algorithmes de classification. Une fois satisfait avec les algorithmes, vous allez les
utiliser pur prédire les résultats (si la personne quittera l’entreprise ou non) dans l'ensemble de test
(test.csv).

**Description des variables**

- Id - numéro d'identification unique
- satisfaction_level - niveau de satisfaction de l’employé
- last_evaluation - score dans la dernière évaluation
- number_project - le nombre de projet dans lesquels l’employée participe
- average_montly_hours - la moyenne des heures mensuelles
- time_spend_company - temps avec l'eterprise
- Work_accident - accident du travail
- left - quitter l’entreprise - variable cible
- promotion_last_5years - une promotion au cours de 5 dernières années
- department - le département
- salary - niveau de salaire

## But du TP

- Dans ce premier TP, vous devez effectuer une analyse exploratoire et qualitative des données afin de mieux comprendre les données et les différentes variables.
- Vous allez utiliser R pour effectuer l'analyse et développer les scripts nécessaires.
- Sur la base des résultats de l'analyse, nous devrons rédiger un petit rapport, dans lequel vous livrerez ce qui est demandé ci-dessous, et décrire ce qui, selon votre analyse, sont les facteurs les plus importants qui déterminent si un employé quitte ou reste.

---

# TP01

## Analyse préliminaire

Avant de commencer l'analyse, vous devez comprendre votre ensemble de données. Pour ce faire, vous devez répondre clairement aux questions suivantes dans votre rapport :

- **Combien y a-t-il d'instances et de variables (attributs) ?**
- **Quelle est la variable cible et est-elle quantitative ou qualitative ?**
- **Les autres attributs sont-ils quantitatifs ou qualitatifs ?**
- **Certaines variables doivent-elles être exclues de l'analyse ?**
- **Pourquoi ? Y a-t-il des données manquantes ?**

## Analyse exploratoire

Pour chaque attribut **qualitatif f** :

- **calculer la distribution de probabilité *P* (f )**
- calculer la probabilité conditionnelle de la variable cible, ***y***, compte tenu des valeurs d'attribut *P* (**y|f** )
- Choisissez une variable qualitative, **f ,**et la variable cible ***y*** et montrez des exemples de la façon dont les règles de probabilité suivantes s'appliquent à elles :
    
    *P* (*f, y*) = *P* (*f|y*)*PyY* ) = *P* (*y|f* )*P* (*f* ), connue sous le nom de règle de multiplication.
    
    *P* (*y|f* ) = *P* (*f|y*)*P* (*y*)*/P* (*f* ), connue sous le nom de règle de Bayes. 
    
    à l'aide des matrices de probabilité que vous avez établies ci-dessus et de la matrice de contiguïté.
    
    A l'aide de vos exemples, expliquez la signification de chacun des termes que vous voyez ci-dessus, c ' est-à-dire *P* (*f, y*)*, P* (*f|y*)*, P* (*y*)*, ...* etc.