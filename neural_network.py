#!/usr/bin/env python3
# Nick Sweeting 2016-03-15
# Simple Neural network based on http://lumiverse.io/video/part-2-forward-propagation

from collections import defaultdict
import numpy as np


def array_dimensions(array):
    """[[1,3,4],
        [2,6,5]] -> (2, 3)"""
    x = len(array[0]) if array and hasattr(array[0], '__iter__') else 0
    y = len(array)
    return y, x

def generate_array(y, x, rand=False):
    """(2, 1) -> [[None], [None]]"""
    if rand:
        return np.random.randn(y, x)
    array = []
    for _ in range(y):
        array.append([None * x])


def sigmoid(z):
    """simple sigmoid activation function"""
    return 1 / (1 + np.exp(-z))

def sigmoid_derivative(z):
    """derivative of the activation function"""
    return np.exp(-z) / ((1 + np.exp(-z)) ** 2)

def cost(actual, expected):
    """euclidian distance cost (loss) function for gradient descent"""
    return 0.5 * sum((expected - actual) ** 2)


class NeuralNetwork(object):
    """A generic multi-level deep-learning neural network"""

    default_hyperparameters = {
        'layers': [2, 4, 1],  # input to output, number of nodes in each layer
        'weights': [
            [[1, 1, 1, 1], [1, 1, 1, 1]],  # layer 0 -> layer 1
            [[1], [1], [1], [1]],          # layer 1 -> layer 2
        ],
    }

    def __init__(self, layers=None, weights=None, activation_func=sigmoid, cost_func=cost):
        self.activation_func = activation_func
        self.cost_func = cost_func
        self.layers = layers or self.default_hyperparameters['layers']
        self.weights = weights or self.default_hyperparameters['weights']
        self.a = []  # layer activity
        self.z = []  # weighted layer inputs
        self.stats = defaultdict(int)

        # safety checks
        assert self.layers and self.weights, \
            'Missing layer dimensions or initial weights'

        assert len(self.layers) - 1 == len(self.weights), \
            'Number of weight matrixes does not match number of layers'

        # check weight matrix dimensions match layer dimensions
        for layer, layer_nodes in enumerate(self.layers):
            if layer == 0: continue

            weights = self.weights[layer - 1]
            prev_layer_nodes = self.layers[layer - 1]

            assert array_dimensions(weights) == (prev_layer_nodes, layer_nodes), \
                'Layer %s weights %s do not match number of nodes (%s)' % (
                    layer+1, weights, self.layers[layer]
                )

    def __str__(self):
        layers_str = '->'.join(str(l) for l in self.layers)
        # stats_str = ', '.join('%s: %s' % (k,v ) for k, v in self.stats.items())

        return 'NN: {0} # ♻️  {1} # {2} ⚖ '.format(
            layers_str, self.stats['forwards'], self.weights)

    def __repr__(self):
        return 'NeuralNet(layers={0}, stats={1})'.format(self.layers, self.stats)

    def apply_weights(self, input, weights):
        """get the dot product of the weights applied to inputs"""
        return np.dot(input, weights)

    def apply_activation(self, values, func=None):
        """apply the activation function which sums the inputs"""
        return func(values) if func else self.activation_func(values)

    def forward(self, values):
        """propagate values through the network iteratively"""

        # feed input into the first layer
        self.a.append(values)

        for layer, _ in enumerate(self.layers[1:]):
            # apply weights to layer inputs (a for activity)
            a, weights = self.a[layer], self.weights[layer]
            z = self.apply_weights(a, weights)
            self.z.append(z)

            # apply activation function to weighted inputs
            self.a.append(self.apply_activation(z))
            self.stats['forwards'] += 1

        # return the output layer activity
        return self.a[-1]


    def cost_derivative(self, actual, expected, layer_idx):
        layer_delta = np.multiply(
            -(expected - actual),
            sigmoid_derivative(self.z[2]),
        )

        zs = np.array(self.a[1])
        dJW2 = np.dot(zs.T, layer_delta)


    def train(self):
        for layer, layer_nodes in enumerate(self.layers):
            if not layer: continue
            prev_layer_nodes = self.layers[layer - 1]

            weights = self.weights[layer - 1]
            y, x = array_dimensions(weights)


if __name__ == '__main__':
    hyperparameters = {}  # e.g. 'layers': [2, 4, 1]
    inputs = [
        [3, 5],
        [5, 1],
        [10, 2],
    ]
    expected_outputs = [
        [75],
        [82],
        [93],
    ]

    network = NeuralNetwork(**hyperparameters)
    print(network.forward(inputs))
    print(network.__str__())
    print(network.__repr__())
