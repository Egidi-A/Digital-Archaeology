
```bash
source venv/bin/activate

python gemini_java_generator.py
# oppure
python python/traduttoreDirettoWithSQL_generator.py --cobol resources/cobol/payroll_system_cobol.cbl --sql resources/sql/payroll_schema.sql

javac GestioneConti.java
java -cp .:postgresql-42.7.3.jar GestioneConti

python python/generazioneUnitTest.py --java output/Gestione-Conti/GestioneConti.java
```
il postgresql-42.7.3.jar deve essere messo nella stessa cartella del file GestioneConti.java
dopo che è stato generato il file .java, modificare la database section con i dati del database già creato e popolato (o già esistente)

spostare il codice fenerato dentro a cartella creata apposta in cui metto dentro il jar
runnare il javac
runnare il java

implementazioni future:
- gestione automatizzata del passaggio nelle cartelle dei file generati e necessari alla run
- gestione automatizzata del jar (?)
- gestione automaticcata del javac
- gestione automatizzata del java

