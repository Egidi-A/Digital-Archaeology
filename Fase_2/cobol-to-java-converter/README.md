
```bash
source venv/bin/activate

python python/traduttoreDirettoWithSQL_generator.py
# oppure
python python/traduttoreDirettoWithSQL_generator.py --cobol resources/cobol/bank_system_cobol.cbl --sql resources/sql/bank_schema.sql

javac GestioneConti.java
java -cp .:postgresql-42.7.3.jar GestioneConti
```


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


# Rendi eseguibile lo script (solo la prima volta)
chmod +x java_to_jar.py

# Esegui lo script
python python/java_to_jar.py trials/Gestione-Conti-1/GestioneConti.java --gemini-api-key YOUR_API_KEY

# O con un nome di progetto personalizzato
python python/java_to_jar.py trials/Gestione-Conti-1/GestioneConti.java --project-name PacchettizzazioneConti --gemini-api-key YOUR_API_KEY



// DA MIGLIORARE A 24/06
-- api key non esplicita
