---
inclusion: manual
---

# Phase 2: COBOL to Java Migration

You are migrating COBOL code to Java for cloud deployment.

## Prerequisites

- Phase 1 analysis completed
- Data structure mappings available
- Business logic documented

## Step 2.1: Java Project Setup

### Tasks

1. **Create Maven/Gradle project structure:**
   ```
   project-root/
   ├── src/
   │   ├── main/
   │   │   ├── java/
   │   │   │   └── com/migration/cobol/
   │   │   │       ├── model/        (data classes)
   │   │   │       ├── service/      (business logic)
   │   │   │       ├── repository/   (data access)
   │   │   │       ├── util/         (utilities)
   │   │   │       └── legacy/       (COBOL compatibility)
   │   │   └── resources/
   │   │       ├── application.properties
   │   │       └── db/migration/     (Flyway scripts)
   │   └── test/
   │       └── java/
   ├── pom.xml or build.gradle
   └── README.md
   ```

2. **Configure dependencies:**
   - Java 17 or later
   - Spring Boot 3.x (for services/APIs)
   - JUnit 5 + Mockito (testing)
   - Lombok (reduce boilerplate)
   - MapStruct (data mapping)
   - SLF4J + Logback (logging)
   - Database: JDBC, JPA/Hibernate
   - File I/O: Apache Commons IO
   - COBOL data: cb2java or custom handlers
   - Validation: javax.validation
   - BigDecimal for precision arithmetic

3. **Create configuration files:**
   - pom.xml or build.gradle with all dependencies
   - application.properties/yml
   - logback.xml
   - .gitignore

### Output

- Complete Java project structure
- Build configuration file
- README with setup instructions

## Step 2.2: Convert Data Structures to Java

### Tasks

1. **Create Java POJOs for each COBOL record:**
   - Use appropriate Java types from Phase 1 mapping
   - Add Lombok annotations: @Data, @Builder, @NoArgsConstructor, @AllArgsConstructor
   - Include javax.validation annotations (@NotNull, @Size, @Pattern)
   - Add JavaDoc with COBOL origin references
   - Preserve field order from COBOL

2. **Handle COBOL-specific features:**
   - OCCURS → List<> or arrays (document max size)
   - REDEFINES → Create union types or separate classes with conversion methods
   - COMP-3 packed decimals → BigDecimal with scale preservation
   - Level 88 conditions → enum or boolean methods
   - Group items → nested classes

3. **Create CobolDataConverter utility:**
   ```java
   public class CobolDataConverter {
       // COMP-3 packed decimal conversion
       public static BigDecimal unpackComp3(byte[] packed);
       public static byte[] packComp3(BigDecimal value);
       
       // EBCDIC conversion
       public static String ebcdicToAscii(byte[] ebcdic);
       public static byte[] asciiToEbcdic(String ascii);
       
       // Date conversions
       public static LocalDate cobolDateToJava(String cobolDate, String format);
       public static String javaDateToCobol(LocalDate date, String format);
       
       // String operations
       public static String padRight(String str, int length);
       public static String padLeft(String str, int length, char padChar);
   }
   ```

4. **Organize classes:**
   - Place in com.migration.cobol.model package
   - Group related classes in sub-packages
   - Create package-info.java with documentation

### Output

- Java POJO classes for all COBOL data structures
- CobolDataConverter utility class
- Unit tests for data conversion
- Documentation of type mappings

## Step 2.3: Convert Program Logic to Java

### Tasks

1. **Create service class for each COBOL program:**
   - Class name: [ProgramName]Service
   - Implement business logic methods
   - Use @Service annotation (Spring)
   - Inject dependencies via constructor

2. **Convert COBOL control structures:**
   - PERFORM UNTIL → while loops or Stream operations
   - PERFORM VARYING → for loops or IntStream
   - EVALUATE → switch statements or strategy pattern
   - IF-ELSE → if-else or ternary operators
   - COMPUTE → BigDecimal arithmetic (preserve precision!)
   - GO TO → refactor to structured code

3. **Preserve COBOL behavior:**
   - Use BigDecimal for all financial calculations
   - Match COBOL rounding rules (ROUNDED mode)
   - Preserve truncation behavior
   - Maintain same validation rules
   - Keep error handling logic
   - Match control flow exactly

4. **Implement COBOL operations:**
   - MOVE → assignment or mapping
   - STRING/UNSTRING → StringBuilder or String methods
   - INSPECT → String manipulation
   - SEARCH → List.stream().filter()
   - SORT → Collections.sort() or Stream.sorted()

5. **Add modern Java features:**
   - Comprehensive error handling with custom exceptions
   - Logging at key decision points (SLF4J)
   - Input validation
   - Comments referencing original COBOL line numbers
   - JavaDoc for all public methods

### Output

- Service classes for each COBOL program
- Unit tests for each method (JUnit 5)
- Integration tests for full workflows
- JavaDoc documentation

## Step 2.4: Convert File I/O to Java

### Tasks

1. **For sequential files:**
   - Create FileHandler class using BufferedReader/Writer
   - Implement record-by-record processing
   - Handle fixed-length records (use String.substring)
   - Handle delimited records (use String.split or CSV library)
   - Preserve COBOL record layout exactly

2. **For indexed files (VSAM):**
   - Design database table schema
   - Create JPA entities with @Entity, @Table, @Id
   - Create Spring Data JPA repositories
   - Implement key-based access methods
   - Add appropriate indexes (@Index annotations)

3. **Convert COBOL file operations:**
   - OPEN INPUT/OUTPUT/I-O → file/connection initialization
   - READ → read methods with EOF handling
   - WRITE → write methods
   - REWRITE → update operations
   - DELETE → delete operations
   - CLOSE → resource cleanup (use try-with-resources)
   - AT END → check for null or EOF condition

4. **Error handling:**
   - File not found → FileNotFoundException
   - I/O errors → IOException
   - Data format errors → custom ParseException
   - AT END → return Optional or null
   - FILE STATUS → exception handling

5. **Create file handlers:**
   ```java
   public class CobolFileHandler {
       public List<RecordType> readSequentialFile(Path filePath);
       public void writeSequentialFile(Path filePath, List<RecordType> records);
       public RecordType readFixedLengthRecord(BufferedReader reader);
       public void writeFixedLengthRecord(BufferedWriter writer, RecordType record);
   }
   ```

### Output

- File handler classes
- JPA entities for indexed files
- Repository interfaces
- Unit tests with sample data files
- Integration tests

## Step 2.5: Convert Database Operations to Java

### Tasks

1. **Analyze EXEC SQL statements:**
   - Extract all SQL operations
   - Identify tables and columns
   - Map host variables to method parameters
   - Document cursor operations

2. **Create JPA entities:**
   - @Entity for each table
   - @Table with schema and table name
   - @Id for primary keys
   - @Column with nullable, length, precision
   - @OneToMany, @ManyToOne for relationships
   - Match COBOL data types exactly

3. **Create repository interfaces:**
   ```java
   public interface CustomerRepository extends JpaRepository<Customer, Long> {
       Optional<Customer> findByCustomerId(String customerId);
       List<Customer> findByStatus(String status);
       @Query("SELECT c FROM Customer c WHERE c.balance > :amount")
       List<Customer> findHighBalanceCustomers(@Param("amount") BigDecimal amount);
   }
   ```

4. **Convert SQL operations:**
   - SELECT single row → repository.findById() or custom findBy method
   - SELECT multiple rows → repository.findAll() or custom query
   - INSERT → repository.save(entity)
   - UPDATE → load entity, modify, repository.save()
   - DELETE → repository.deleteById() or repository.delete()
   - Cursors → @Query with pagination or Stream<Entity>

5. **Handle COBOL SQL patterns:**
   - Host variables → method parameters
   - WHENEVER SQLERROR → try-catch with @Transactional
   - WHENEVER NOT FOUND → Optional return types
   - Multi-row fetches → List<Entity> or Page<Entity>
   - COMMIT/ROLLBACK → @Transactional boundaries

6. **Create database migrations:**
   - Flyway or Liquibase scripts
   - V1__create_tables.sql
   - V2__add_indexes.sql
   - Include in src/main/resources/db/migration

### Output

- JPA entities for all tables
- Repository interfaces
- Database migration scripts
- Integration tests with test database (H2 or Testcontainers)
- Database configuration

## Validation Checklist

Before completing Phase 2:
- [ ] Java project builds successfully
- [ ] All data structures converted
- [ ] All business logic migrated
- [ ] File I/O operations implemented
- [ ] Database operations implemented
- [ ] Unit tests passing
- [ ] Integration tests passing
- [ ] Code follows Java best practices
- [ ] Documentation complete

## Next Phase

Proceed to Phase 4 (Testing & Validation)
