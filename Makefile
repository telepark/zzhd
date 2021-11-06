ROOT = ../..
PROJECT = zzhd
KZ_VERSION = $(shell grep vsn src/zzhd.app.src | awk -F\" '{print $$2}')

all: check_mysql_deps check_epgsql_deps check_emysql_deps compile

check_mysql_deps:
	if grep -q "mysql-otp" $(ROOT)/make/deps.mk; then \
	   echo "mysql exists"; \
	else \
	  echo "" >> $(ROOT)/make/deps.mk; \
	  echo "DEPS += mysql mysql_poolboy" >> $(ROOT)/make/deps.mk; \
	  echo "dep_mysql = git https://github.com/mysql-otp/mysql-otp 1.3.0" >> $(ROOT)/make/deps.mk; \
	  echo "dep_mysql_poolboy = git https://github.com/mysql-otp/mysql-otp-poolboy 0.1.7" >> $(ROOT)/make/deps.mk; \
	  echo "" >> $(ROOT)/make/deps.mk; \
	fi

check_epgsql_deps:
	if grep -q "epgsql" $(ROOT)/make/deps.mk; then \
	   echo "epgsql exists"; \
	else \
	  echo "" >> $(ROOT)/make/deps.mk; \
	  echo "DEPS += epgsql dep_pgapp" >> $(ROOT)/make/deps.mk; \
	  echo "dep_epgsql = git https://github.com/epgsql/epgsql" >> $(ROOT)/make/deps.mk; \
	  echo "dep_pgapp = git https://github.com/epgsql/pgapp" >> $(ROOT)/make/deps.mk; \
	  echo "" >> $(ROOT)/make/deps.mk; \
	fi

check_emysql_deps:
	if grep -q "emysql" $(ROOT)/make/deps.mk; then \
	   echo "emysql exists"; \
	else \
	  echo "" >> $(ROOT)/make/deps.mk; \
	  echo "DEPS += emysql" >> $(ROOT)/make/deps.mk; \
	  echo "dep_emysql = git https://github.com/inaka/Emysql.git" >> $(ROOT)/make/deps.mk; \
	  echo "" >> $(ROOT)/make/deps.mk; \
	fi

include $(ROOT)/make/kz.mk

