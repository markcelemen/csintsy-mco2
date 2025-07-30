# FamiLink: The Family Relationship Chatbot

## Setup and Installation

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

---

## How to Use

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

## Project Files

* `family_gui.py` — The main file for the graphical user interface. Run this for the best user experience.
* `family_chatbot.py` — Core Python logic for the chatbot, including input parsing and Prolog integration.
* `family_relationships.pl` — The Prolog knowledge base defining family rules, inferences, and constraints.
* `README.md` — This file.

---

```
