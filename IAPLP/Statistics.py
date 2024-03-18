
from typing import List
from collections import Counter
from LinearAlgebrae import dot,make_matrix,print_matrix

# --- Central Tendencies ---

# Média

def mean(xs:List[float]) -> float:
    return sum(xs)/len(xs)

# Mediana ímpar

def median_odd(xs: List[float]) -> float:
    return sorted(xs)[len(xs)//2]

# Mediana par
def median_even(xs: List[float]) -> float:
    sorted_xs = sorted(xs)
    return (sorted_xs[len(xs)//2] + sorted_xs[(len(xs)//2)-1])/2

# Mediana

def median(xs:List[float]) -> float:
    return median_even(xs) if len(xs)%2 == 0 else median_odd(xs)

# Quantil

def quantile(xs:List[float], q:float) -> float:
    q_index = int(q*len(xs))
    return sorted(xs)[q_index]

# Moda

def mode(xs: List[float]) -> List[float]:
    counter = Counter(xs)
    max_counts = [max(counter.values())]
    return [xi for xi,count in counter.items() if count in max_counts]

# --- Dispersion ---

# Amplitude

def data_range(xs:List[float]) -> float:
    return max(xs) - min(xs)

def de_mean(xs:List[float]) -> List[float]:
    x_mean = mean(xs)
    return [x-x_mean for x in xs]

from LinearAlgebrae import sum_of_squares

# Variância

def variance(xs:List[float]) -> float:
    assert len(xs) >= 2, "Variance requires at least two elements"

    return sum_of_squares(de_mean(xs))/(len(xs)-1)

import math

# Desvio padrão

def standard_deviation(xs:List[float]) -> float:
    return math.sqrt(variance(xs))

# Interquantil

def interquantile_change(xs:List[float],i:float,j:float) -> float:
    return abs(quantile(xs,i) - quantile(xs,j)) 

# --- Correlation ---

# Covariância

def covariance(xs:List[float], ys:List[float]) -> float:
    assert len(xs) == len(ys), "The two lists must be the same lenght"
    return(dot(de_mean(xs),de_mean(ys))/(len(xs)-1))

# Correlação

def correlation(xs: List[float], ys: List[float]) -> float:
    stdev_x = standard_deviation(xs)
    stdev_y = standard_deviation(ys)
    if stdev_x > 0 and stdev_y > 0:
        return covariance(xs, ys) / (stdev_x * stdev_y)
    else:
        return 0
    
# Coef. de Associação (Variáveis Qualitativas)

def double_entry_table(xs:List[str],ys:List[str])-> List[List]:
    assert len(xs) == len(ys), 'The two lists must have the same lenght'
    x_counter, y_counter = Counter(xs),Counter(ys)

    combinations = [(xs[i],ys[i]) for i in range(len(xs))]
    set_x, set_y = list(x_counter.keys()),list( y_counter.keys())
    relations = Counter(combinations)
    m = make_matrix(len(set_x)+2,len(set_y)+2)

    m[0][0] = r'V\Y'
    m[-1][0] = 'Total'
    m[0][-1] = 'Total'

    for i in range(len(set_x)):
        m[0][i+1] = set_x[i]
    
    for j in range(len(set_y)):
        m[j+1][0] = set_y[j]

    for i in range(1, len(m)):
        for j in range(1, len(m[0])): 
            new_value = 0

            x_var, y_var = m[0][j], m[i][0]
            relation = (x_var, y_var)
            if relation in relations.keys():
                new_value = relations[relation]
            elif x_var == 'Total' and y_var == 'Total':
                new_value = len(xs)
            elif x_var == 'Total':
                new_value = y_counter[y_var]
            elif y_var == 'Total':
                new_value = x_counter[x_var]

            m[i][j] = new_value

    return m

def extract_numbers(m:List[List]) -> List[List[int]]:
    num_matrix = []

    # Itera sobre a matriz original e adiciona os números à lista num_matrix
    for row in m:
        num_row = []
        for item in row:
            if isinstance(item, int):  # Verifica se o item é um número
                num_row.append(item)
        if num_row:  # Verifica se a linha tem números
            num_matrix.append(num_row)

    return num_matrix

def square_qui(xs: List[str], ys: List[str]) -> float:
    assert len(xs) == len(ys), 'The two lists must have the same lenght'
    m = extract_numbers(double_entry_table(xs,ys))

    sum = 0
    for i in range(len(m)-1):
        for j in range(len(m[0])-1):
            n_star = (m[i][-1] * m[-1][j])/m[-1][-1]
            sum += ((m[i][j] - n_star)**2)/n_star

    return sum

def association():
    ...


    
    


