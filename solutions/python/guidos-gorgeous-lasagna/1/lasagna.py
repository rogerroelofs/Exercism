"""Functions used in preparing Guido's gorgeous lasagna.

Learn about Guido, the creator of the Python language:
https://en.wikipedia.org/wiki/Guido_van_Rossum

This is a module docstring, used to describe the functionality
of a module and its functions and/or classes.
"""

EXPECTED_BAKE_TIME = 40
LAYER_PREPARATION_TIME = 2

def bake_time_remaining(time_elapsed: int) -> int:
    """Calculate the bake time remaining.

    :param elapsed_bake_time: int - baking time already elapsed.
    :return: int - remaining bake time (in minutes) derived from 'EXPECTED_BAKE_TIME'.

    Function that takes the actual minutes the lasagna has been in the oven as
    an argument and returns how many minutes the lasagna still needs to bake
    based on the `EXPECTED_BAKE_TIME`.
    """
    return EXPECTED_BAKE_TIME - time_elapsed


def preparation_time_in_minutes(layers: int) -> int:
    """Calculate the preparation time.

    :param layers: int - number of layers in the lasagna.
    :return: int - preparation time (in minutes) derived from 'PREPARATION_TIME'.

    Function that takes the number of layers you want to add to the lasagna as
    an argument and returns how many minutes you would spend making them, based
    on the `LAYER_PREPARATION_TIME` per layer.
    """
    return LAYER_PREPARATION_TIME * layers


def elapsed_time_in_minutes(layers: int, time_elapsed: int) -> int:
    """Calculate the elapsed time.

    :param layers: int - number of layers in the lasagna.
    :param time_elapsed: int - baking time already elapsed.
    :return: int - elapsed time (in minutes) derived from 'PREPARATION_TIME' and 'time_elapsed'.

    Function that takes the number of layers you want to add to the lasagna as
    an argument and returns how many minutes you would spend making them, based
    on the `LAYER_PREPARATION_TIME` per layer.
    """
    return preparation_time_in_minutes(layers) + time_elapsed