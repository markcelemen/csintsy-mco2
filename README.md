# FamiLink: The Family Relationship Chatbot

**Authors:**  
**Date:** July 31, 2025

---

## 1. Overview

**FamiLink** is a chatbot designed to understand and reason about complex family relationships. Users can input facts about a family in natural language, and FamiLink will build a knowledge base. It can then answer questions about both direct and inferred relationships (like cousins, uncles, and grandparents) and will reject any statements that create a logical contradiction (such as incest, circular dependencies, or violating defined rules like monogamy).

The project uses a powerful combination of **Python** for the user interface and natural language processing, and **Prolog** for the back-end logical inference and constraint checking.

---

## 2. Key Features

- **Natural Language Processing**  
  Accepts simple English statements and questions as input.

- **Logical Inference**  
  Can deduce complex relationships (siblings, grandparents, aunts, uncles, cousins, etc.) from basic facts.

- **Constraint Checking**  
  Actively prevents logical paradoxes and contradictions, such as:
  - A person being their own parent or ancestor.
  - A person having more than two parents or more than one spouse.
  - Parents or spouses of the same gender.
  - Marriage or children between close blood relatives (parents, siblings, etc.).
  - Marriage to close in-laws (parent-in-law, child-in-law).

- **Interactive Interface**  
  Available as both a user-friendly **Graphical User Interface (GUI)** and a **command-line application**.

- **Persistent Knowledge**  
  Maintains state, allowing for an ongoing conversation.

---

## 3. Technologies Used

- **Front-end & Core Logic:** Python 3  
  - `tkinter` for the Graphical User Interface

- **Back-end Reasoning Engine:** SWI-Prolog

- **Python-Prolog Bridge:** `pyswip` library

---

## 4. Setup and Installation

### Prerequisites

- **Python 3**  
  Make sure Python 3 is installed. Download from: [https://www.python.org](https://www.python.org)

- **SWI-Prolog**  
  Required for the logical engine. Download from: [https://www.swi-prolog.org](https://www.swi-prolog.org)  
  > **Important (Windows):** During installation, check the box that says “Add swipl to the system PATH for all users”.

### Installation Steps

1. **Clone or Download the Project**  
   Place all the project files (`family_gui.py`, `family_chatbot.py`, `family_relationships.pl`, and `README.md`) into a single directory.

2. **Install the `pyswip` library**  
   Open your terminal and run:

   ```bash
   pip install pyswip
````

## 5. How to Use

You can interact with FamiLink through the **GUI** or the **command-line interface**.

### Running the GUI Version (Recommended)

To start the graphical interface:

```bash
python family_gui.py
```

A chat window will appear. You can start entering statements and questions.

### Running the Command-Line Version

For a console-based experience:

```bash
python family_chatbot.py
```

* Type `help` to see a list of available commands and sentence structures.
* Type `exit` to quit.

---

## 6. Project Files

* `family_gui.py` — The main file for the graphical user interface. Run this for the best user experience.
* `family_chatbot.py` — Core Python logic for the chatbot, including input parsing and Prolog integration.
* `family_relationships.pl` — The Prolog knowledge base defining family rules, inferences, and constraints.
* `README.md` — This file.

---

```
