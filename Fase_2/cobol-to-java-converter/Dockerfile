# Build stage
FROM maven:3.8-openjdk-11 AS build
WORKDIR /app

# Copy pom.xml and download dependencies
COPY pom.xml .
RUN mvn dependency:go-offline

# Copy source code
COPY src ./src

# Build the application
RUN mvn clean package

# Runtime stage
FROM openjdk:11-jre-slim
WORKDIR /app

# Copy the JAR from build stage
COPY --from=build /app/target/cobol-to-java-converter-1.0.0.jar app.jar

# Create directories for input/output
RUN mkdir -p /input /output

# Set the entrypoint
ENTRYPOINT ["java", "-jar", "app.jar"]

# Default command (can be overridden)
CMD ["/input/sample.cob", "/output/output.java"]