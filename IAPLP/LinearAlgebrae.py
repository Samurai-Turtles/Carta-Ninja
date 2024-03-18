from typing import List

# --- Vectors --- 

Vector = List[float]

# Soma entre vetores

def add(v:Vector , w:Vector) -> Vector:
    assert len(v) == len(w), "Vectors must be the same lenght"

    return [vi + wi for vi,wi in zip(v,w)]

# Subtração de vetores

def subtract(v:Vector, w:Vector) -> Vector:
    assert len(v) == len(w), "Vectors must be the same lenght"

    return [vi - wi for vi,wi in zip(v,w)]

# Soma dos elementos nas posições i dos vetores

def vector_sum(vectors: List[Vector]) -> Vector:
    assert vectors, "Null vectors"

    num_elements = len(vectors[0])
    assert all(len(v) == num_elements for v in vectors), "All vectors must be the same size"

    return [sum(vector[i] for vector in vectors) for i in range(num_elements)]

# Multiplicação por escalar

def scalar_multiply(v: Vector, scalar: int) -> Vector:
    return [value*scalar for value in v]

# Média do vetor

def vector_mean(vectors: List[Vector]) ->Vector:
    return scalar_multiply(vector_sum(vectors),1/len(vectors))

# Multiplicação escalar entre vetores

def dot(v: Vector, w:Vector) -> float:
    return sum([vi * wi for vi,wi in zip(v,w)])

# Quadrado escalar do vetor

def sum_of_squares(v:Vector) -> float:
    return dot(v,v)

import math

# Magnetude (tamanho) do vetor

def magnitude(v:Vector) -> float:
    return math.sqrt(sum_of_squares(v))

# Distância entre dois vetores

def distance(v:Vector, w:Vector) -> float:
    return magnitude(subtract(v,w))

from typing import Tuple

# --- Matrices ---

Matrix = List[List[float]]

# Razão da matriz

def shape(m:Matrix) -> Tuple[int,int]:
    l = len(m) 
    c = len(m[0]) if m else 0
    return l,c

# Obtém a linha i

def get_row(m:Matrix, i:int) -> List[float]:
    assert m, 'Invalid matrix'
    assert i < len(m), 'Invalid idex'
    return m[i]

# Obtém a coluna i

def get_column(m:Matrix, i:int) -> List[float]:
    assert m, 'Invalid matrix'
    assert i < len(m[0]), 'Invalid index'
    return [row[i] for row in m]

from typing import Callable

# Cria uma matriz com r linhas e c colunas

def make_matrix(r: int, c: int, entry_fn:Callable = lambda i,j:0) -> Matrix:
    return [[entry_fn(i,j) for j in range(c)] for i in range(r)]

def print_matrix(matrix):
    for row in matrix:
        for element in row:
            print(element, end=' ')  # Imprime o elemento seguido de um espaço
        print()  # Imprime uma nova linha após cada linha da matriz

# Cria uma matriz identidade com n linhas e n colunas

def identity_matrix(n:int) -> Matrix:
    return make_matrix(n,n, lambda i,j: 1 if i == j else 0)



