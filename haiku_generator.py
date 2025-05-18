import random
import datetime
import os

from enum import Enum
from typing import Dict


class Mood(Enum):
    Happy = 0
    Sad = 1
    Reflective = 2
    Anxious = 3
    Nostalgic = 4


class Theme(Enum):
    Rain = 0
    Forest = 1
    Night = 2
    City = 3
    Ocean = 4
    Mountain = 5
    Desert = 6


class HaikuHistory:
    def __init__(self, text, mood, theme, timestamp):
        self.text = text
        self.mood = mood
        self.theme = theme
        self.timestamp = timestamp

    def __str__(self):
        return f"{self.timestamp} - {self.mood.name} - {self.theme.name}:\n{self.text}"


haiku_history: Dict[int, HaikuHistory] = {}


def random_from(xs):
    idx = random.randint(0, len(xs))
    return xs[idx]


def prompt_choice(label, enum_cls):
    print("\n" + label)
    options = sorted(enum_cls, key=lambda x: x.value)
    for i, opt in enumerate(options, 1):
        print(f"{i}. {opt.name}")
    idx = int(input("Zadej číslo (nebo 0 pro náhodný výběr): "))
    if idx == 0:
        return random_from(options)
    elif 1 <= idx <= len(options):
        return options[idx - 1]
    else:
        print("Neplatná volba, zkus to znovu.")
        return prompt_choice(label, enum_cls)


def validate_haiku(poem):
    lines = [line.split() for line in poem.splitlines()]
    counts = [len(l) for l in lines]
    return len(counts) == 3 and counts == [5, 7, 5]


def save_haiku(poem, mood, theme):
    current_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    new_id = len(haiku_history) + 1
    entry = HaikuHistory(poem, mood, theme, current_time)
    haiku_history[new_id] = entry
    return new_id


def export_haiku(poem, mood, theme):
    current_time = datetime.datetime.now()
    timestamp_str = current_time.strftime("%Y%m%d%H%M%S")
    filename = f"haiku_{mood.name}_{theme.name}_{timestamp_str}.txt"
    folder = "output"
    os.makedirs(folder, exist_ok=True)
    with open(os.path.join(folder, filename), "w") as f:
        f.write("Haiku\n")
        f.write(f"Mood: {mood.name}\n")
        f.write(f"Theme: {theme.name}\n")
        f.write(f"Date: {current_time.strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        f.write(poem)
    print(f"Haiku uloženo do souboru: {filename}")


def get_word_lists(mood, theme):
    if mood == Mood.Happy and theme == Theme.Forest:
        return (
            ["Morning sun breaks through", "Laughing leaves dancing"],
            ["Squirrels leap on mossy trunks", "Birdsongs echo far and wide"],
            ["Joy hides in green shade", "Peace returns with wind"]
        )
    elif mood == Mood.Sad and theme == Theme.Night:
        return (
            ["Empty streets at dusk", "Cold wind sighs alone"],
            ["Darkness swallows silent stars", "Footsteps fade into the void"],
            ["Nothing left to say", "Tears fall with the moon"]
        )
    elif mood == Mood.Reflective and theme == Theme.Rain:
        return (
            ["Drops paint the window", "Stillness in my breath"],
            ["Thoughts like rivers slowly pass", "..."],
            ["...", "..."]
        )
    else:
        return ([], [], [])
