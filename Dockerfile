# Stage 1: Build da aplicação com Maven
FROM maven:3.9.4-eclipse-temurin-17-alpine AS build

WORKDIR /app

# Copia os arquivos de configuração e código
COPY pom.xml .
COPY src ./src

# Faz o build do .jar (modo release, sem testes para acelerar)
RUN mvn clean package -DskipTests

# Stage 2: Imagem final para rodar a aplicação
FROM eclipse-temurin:17-jdk-alpine

RUN addgroup -S spring && adduser -S spring -G spring

USER spring:spring

WORKDIR /app
# Copia o .jar do estágio anterior
COPY --from=build /app/target/*.jar app.jar

# Define o comando padrão
ENTRYPOINT ["java","-jar","/app/app.jar"]

