# Makefile per gestire l'ambiente Docker COBOL

.PHONY: help build up down restart logs shell clean compile run-conti run-paghe run-magazzino

# Colori per output
GREEN=\033[0;32m
YELLOW=\033[0;33m
RED=\033[0;31m
NC=\033[0m # No Color

help: ## Mostra questo help
	@echo "$(GREEN)COBOL Docker Environment - Comandi disponibili:$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(YELLOW)%-20s$(NC) %s\n", $$1, $$2}'

build: ## Costruisce le immagini Docker
	@echo "$(GREEN)Building Docker images...$(NC)"
	docker-compose build

up: ## Avvia tutti i container
	@echo "$(GREEN)Starting containers...$(NC)"
	docker-compose up -d
	@echo "$(GREEN)Waiting for services to be ready...$(NC)"
	@sleep 10
	@echo "$(GREEN)Environment ready!$(NC)"

down: ## Ferma e rimuove i container
	@echo "$(YELLOW)Stopping containers...$(NC)"
	docker-compose down

restart: down up ## Riavvia tutti i container

logs: ## Mostra i log dei container
	docker-compose logs -f

shell: ## Accede alla shell del container COBOL
	@echo "$(GREEN)Accessing COBOL container...$(NC)"
	docker exec -it cobol_runtime bash

db-shell: ## Accede a PostgreSQL
	@echo "$(GREEN)Accessing PostgreSQL...$(NC)"
	docker exec -it cobol_postgres psql -U postgres

compile: ## Ricompila tutti i programmi COBOL
	@echo "$(GREEN)Recompiling COBOL programs...$(NC)"
	docker exec cobol_runtime /cobol/scripts/compile.sh

run-conti: ## Esegue il programma gestione conti
	@echo "$(GREEN)Running Gestione Conti...$(NC)"
	docker exec -it cobol_runtime /cobol/bin/gestione-conti.sh

run-paghe: ## Esegue il programma gestione paghe
	@echo "$(GREEN)Running Gestione Paghe...$(NC)"
	docker exec -it cobol_runtime /cobol/bin/gestione-paghe.sh

run-magazzino: ## Esegue il programma gestione magazzino
	@echo "$(GREEN)Running Gestione Magazzino...$(NC)"
	docker exec -it cobol_runtime /cobol/bin/gestione-magazzino.sh

clean: ## Pulisce volumi e container
	@echo "$(RED)Cleaning up...$(NC)"
	docker-compose down -v
	rm -rf reports/*
	@echo "$(GREEN)Cleanup complete!$(NC)"

test-db: ## Testa la connessione ai database
	@echo "$(GREEN)Testing database connections...$(NC)"
	@docker exec cobol_postgres psql -U postgres -c "\l"
	@echo "$(GREEN)Database test complete!$(NC)"

backup-db: ## Backup dei database
	@echo "$(GREEN)Creating database backup...$(NC)"
	@mkdir -p backups
	@docker exec cobol_postgres pg_dumpall -U postgres > backups/backup_$$(date +%Y%m%d_%H%M%S).sql
	@echo "$(GREEN)Backup complete!$(NC)"

restore-db: ## Ripristina l'ultimo backup
	@echo "$(YELLOW)Restoring from latest backup...$(NC)"
	@latest_backup=$$(ls -t backups/*.sql | head -1); \
	if [ -n "$$latest_backup" ]; then \
		cat $$latest_backup | docker exec -i cobol_postgres psql -U postgres; \
		echo "$(GREEN)Restore complete from $$latest_backup$(NC)"; \
	else \
		echo "$(RED)No backup found!$(NC)"; \
	fi