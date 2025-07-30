FamiLink: The Family Relationship Chatbot
Authors: 
Date: July 31, 2025

1. Overview
FamiLink is a chatbot designed to understand and reason about complex family relationships. Users can input facts about a family in natural language, and FamiLink will build a knowledge base. It can then answer questions about both direct and inferred relationships (like cousins, uncles, and grandparents) and will reject any statements that create a logical contradiction (such as incest, circular dependencies, or violating defined rules like monogamy).

The project uses a powerful combination of Python for the user interface and natural language processing, and Prolog for the back-end logical inference and constraint checking.

2. Key Features
Natural Language Processing: Accepts simple English statements and questions as input.

Logical Inference: Can deduce complex relationships (siblings, grandparents, aunts, uncles, cousins, etc.) from basic facts.

Constraint Checking: Actively prevents logical paradoxes and contradictions, such as:

A person being their own parent or ancestor.

A person having more than two parents or more than one spouse.

Parents or spouses of the same gender.

Marriage or children between close blood relatives (parents, siblings, etc.).

Marriage to close in-laws (parent-in-law, child-in-law).

Interactive Interface: Available as both a user-friendly Graphical User Interface (GUI) and a command-line application.

Persistent Knowledge: The chatbot maintains its state, allowing for an ongoing conversation.

Robust Testing: Includes a comprehensive, automated test suite to ensure logical integrity.

3. Technologies Used
Front-end & Core Logic: Python 3

tkinter for the Graphical User Interface.

Back-end Reasoning Engine: SWI-Prolog

Python-Prolog Bridge: pyswip library

4. Setup and Installation
To run this project, you need Python and SWI-Prolog installed on your system.

Prerequisites
Python 3: Make sure you have Python 3 installed. You can download it from python.org.

SWI-Prolog: The logical engine requires SWI-Prolog. Download and install it from the official website: swi-prolog.org.

Important (Windows): During installation, make sure to check the box that says "Add swipl to the system PATH for all users".

Installation Steps
Clone or Download the Project:
Place all the project files (family_gui.py, family_chatbot.py, family_relationships.pl, and this README.md) into a single directory.

Install the pyswip library:
Open your command prompt or terminal and run the following command:

pip install pyswip

5. How to Use
You can interact with FamiLink through the GUI or the command-line interface.

Running the GUI Version (Recommended)
To start the graphical interface, navigate to the project directory in your terminal and run:

python family_gui.py

A chat window will appear, and you can start entering statements and questions.

Running the Command-Line Version
For a console-based experience, run the following command:

python family_chatbot.py

Type help to see a list of available commands and sentence structures. Type exit to quit.

6. Project Files
family_gui.py: The main file for the graphical user interface. Run this for the best user experience.

family_chatbot.py: Contains the core Python logic for the chatbot, including input parsing and interaction with the Prolog engine. This file can be run directly for a command-line interface.

family_relationships.pl: The Prolog knowledge base. This file defines all the family relationship rules, inferences, and logical constraints.

README.md: This file.
