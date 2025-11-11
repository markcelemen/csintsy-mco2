# 🧩 FamiLink: The Family Relationship Chatbot

**FamiLink** is an intelligent chatbot that helps users explore and understand family relationships using logical reasoning.
It integrates **Python** and **Prolog** to infer relationships and answer queries about family members.

---

## ⚙️ Setup and Installation

### **Prerequisites**

Before starting, ensure you have the following installed:

* **Python 3**
  Download from [https://www.python.org](https://www.python.org)  

* **SWI-Prolog**
  Required for the logical inference engine.  
  Download from [https://www.swi-prolog.org](https://www.swi-prolog.org)  

  > **Note (Windows Users):**  
  > During installation, check the box that says  
  > **“Add swipl to the system PATH for all users.”**  

---

### **Installation Steps**

1. **Clone or Download the Project**  
   Place all project files in the same directory:

   ```
   family_gui.py
   family_chatbot.py
   family_relationships.pl
   README.md
   ```

2. **Install Required Python Package**  
   Open your terminal or command prompt and run:

   ```bash
   pip install pyswip
   ```

---

## 💬 How to Use

You can interact with **FamiLink** through either the **graphical user interface (GUI)** or the **command-line interface (CLI)**.

### **Running the GUI Version (Recommended)**

Start the graphical chat interface with:

```bash
python family_gui.py
```

A window will appear where you can type statements and ask questions about family relationships.

### **Running the Command-Line Version**

For a console-based experience:

```bash
python family_chatbot.py
```

---

## 📁 Project Files

| File                      | Description                                                                 |
| ------------------------- | --------------------------------------------------------------------------- |
| `family_gui.py`           | Main graphical interface for user interaction. *(Recommended for use)*      |
| `family_chatbot.py`       | Core chatbot logic — handles input parsing and Prolog integration.          |
| `family_relationships.pl` | Prolog knowledge base defining family relationships, rules, and inferences. |
| `README.md`               | This documentation file.                                                    |

---

## 🧠 Example Use Cases

* Identify relationships: *“Who is John’s father?”*
* Add new relationships: *“Mary is the mother of Sam.”*
* Query inferred relations: *“Is Sam the sibling of Anna?”*
