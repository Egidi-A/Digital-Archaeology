
```bash
source venv/bin/activate

python gemini_java_generator.py
# oppure
python translator/traduttoreDirettoWithSQL_generator.py --cobol resources/cobol/payroll_system_cobol.cbl --sql resources/sql/payroll_schema.sql

javac GestioneConti.java
java -cp .:postgresql-42.7.3.jar GestioneConti
```
il postgresql-42.7.3.jar deve essere messo nella stessa cartella del file GestioneConti.java
dopo che è stato generato il file .java, modificare la database section con i dati del proprio database