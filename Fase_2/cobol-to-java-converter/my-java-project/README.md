# Compila il progetto
mvn compile

# Compila i test
mvn test-compile

# Esegui tutti i test
mvn test

# Esegui test specifici
mvn test -Dtest=MyClassTest

# Esegui test con output dettagliato
mvn test -Dtest=MyClassTest -Dmaven.test.failure.ignore=true

# Esegui test e genera report
mvn test jacoco:report