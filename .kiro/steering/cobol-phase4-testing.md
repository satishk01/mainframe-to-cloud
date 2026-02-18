---
inclusion: manual
---

# Phase 4: Testing & Validation

You are creating comprehensive tests to validate the COBOL migration.

## Objectives

1. Create unit tests for all components
2. Create integration tests for workflows
3. Create comparison tests (COBOL vs migrated code)
4. Perform performance testing
5. Validate migration correctness

## Step 4.1: Java Testing

### Unit Tests (JUnit 5)

```java
@ExtendWith(MockitoExtension.class)
class CustomerServiceTest {
    @Mock
    private CustomerRepository repository;
    
    @InjectMocks
    private CustomerService service;
    
    @Test
    @DisplayName("Should calculate balance correctly with COBOL precision")
    void testBalanceCalculation() {
        // Given
        BigDecimal initial = new BigDecimal("1000.00");
        BigDecimal addition = new BigDecimal("250.50");
        BigDecimal expected = new BigDecimal("1250.50");
        
        // When
        BigDecimal result = service.addToBalance(initial, addition);
        
        // Then
        assertEquals(0, expected.compareTo(result));
    }
    
    @Test
    @DisplayName("Should handle COBOL rounding rules")
    void testCobolRounding() {
        // Test COBOL ROUNDED behavior
        BigDecimal value = new BigDecimal("123.456");
        BigDecimal rounded = service.roundCobolStyle(value, 2);
        assertEquals(new BigDecimal("123.46"), rounded);
    }
    
    @ParameterizedTest
    @CsvSource({
        "A, true",
        "I, false",
        "C, false"
    })
    void testAccountStatusValidation(String status, boolean expected) {
        assertEquals(expected, service.isActiveStatus(status));
    }
}
```

### Integration Tests

```java
@SpringBootTest
@Testcontainers
class CustomerIntegrationTest {
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:15")
        .withDatabaseName("testdb");
    
    @Autowired
    private CustomerService service;
    
    @Autowired
    private CustomerRepository repository;
    
    @Test
    @Transactional
    void testFullCustomerWorkflow() {
        // Test complete workflow matching COBOL program
        Customer customer = createTestCustomer();
        Customer saved = service.processCustomer(customer);
        
        assertNotNull(saved.getId());
        assertEquals(customer.getCustomerId(), saved.getCustomerId());
        
        // Verify database state
        Optional<Customer> found = repository.findByCustomerId(customer.getCustomerId());
        assertTrue(found.isPresent());
    }
}
```

### Comparison Tests

```java
@Test
void compareWithCobolOutput() throws Exception {
    // Read COBOL program output
    List<String> cobolOutput = Files.readAllLines(
        Paths.get("test-data/cobol-output.txt")
    );
    
    // Run Java implementation
    List<String> javaOutput = service.processAndFormat(testInput);
    
    // Compare line by line
    assertEquals(cobolOutput.size(), javaOutput.size(), "Output line count mismatch");
    
    for (int i = 0; i < cobolOutput.size(); i++) {
        assertEquals(
            cobolOutput.get(i), 
            javaOutput.get(i),
            "Mismatch at line " + (i + 1)
        );
    }
}
```

### Performance Tests

```java
@Test
@Timeout(value = 5, unit = TimeUnit.SECONDS)
void testPerformanceWithLargeDataset() {
    List<Customer> customers = generateTestCustomers(10000);
    
    long startTime = System.currentTimeMillis();
    service.processBatch(customers);
    long endTime = System.currentTimeMillis();
    
    long duration = endTime - startTime;
    logger.info("Processed 10000 customers in {} ms", duration);
    
    assertTrue(duration < 5000, "Processing took too long");
}
```

## Step 4.2: Node.js Testing

### Unit Tests (Jest)

```typescript
import { CustomerService } from '../services/customer-service';
import { Decimal } from 'decimal.js';

describe('CustomerService', () => {
  let service: CustomerService;

  beforeEach(() => {
    service = new CustomerService();
  });

  describe('balance calculations', () => {
    it('should calculate balance with COBOL precision', () => {
      const initial = new Decimal('1000.00');
      const addition = new Decimal('250.50');
      const expected = new Decimal('1250.50');

      const result = service.addToBalance(initial, addition);

      expect(result.equals(expected)).toBe(true);
    });

    it('should handle COBOL rounding rules', () => {
      const value = new Decimal('123.456');
      const rounded = service.roundCobolStyle(value, 2);
      
      expect(rounded.toString()).toBe('123.46');
    });
  });

  describe('validation', () => {
    it.each([
      ['A', true],
      ['I', false],
      ['C', false],
    ])('should validate status %s as %s', (status, expected) => {
      expect(service.isActiveStatus(status)).toBe(expected);
    });
  });
});
```

### Integration Tests

```typescript
import { CustomerService } from '../services/customer-service';
import { CustomerRepository } from '../repositories/customer-repository';
import { setupTestDatabase, teardownTestDatabase } from './test-helpers';

describe('Customer Integration Tests', () => {
  let service: CustomerService;
  let repository: CustomerRepository;

  beforeAll(async () => {
    await setupTestDatabase();
  });

  afterAll(async () => {
    await teardownTestDatabase();
  });

  beforeEach(async () => {
    repository = new CustomerRepository();
    service = new CustomerService(repository);
  });

  it('should process complete customer workflow', async () => {
    const customer = createTestCustomer();
    
    const saved = await service.processCustomer(customer);
    
    expect(saved.id).toBeDefined();
    expect(saved.customerId).toBe(customer.customerId);

    const found = await repository.findByCustomerId(customer.customerId);
    expect(found).toBeDefined();
  });
});
```

### Comparison Tests

```typescript
import { readFile } from 'fs/promises';
import { CustomerService } from '../services/customer-service';

describe('COBOL Comparison Tests', () => {
  it('should match COBOL output exactly', async () => {
    // Read COBOL program output
    const cobolOutput = await readFile('test-data/cobol-output.txt', 'utf-8');
    const cobolLines = cobolOutput.split('\n');

    // Run Node.js implementation
    const service = new CustomerService();
    const nodeOutput = await service.processAndFormat(testInput);
    const nodeLines = nodeOutput.split('\n');

    expect(nodeLines.length).toBe(cobolLines.length);

    cobolLines.forEach((cobolLine, index) => {
      expect(nodeLines[index]).toBe(cobolLine);
    });
  });
});
```

### Performance Tests

```typescript
describe('Performance Tests', () => {
  it('should process large dataset within time limit', async () => {
    const customers = generateTestCustomers(10000);
    
    const startTime = Date.now();
    await service.processBatch(customers);
    const endTime = Date.now();
    
    const duration = endTime - startTime;
    console.log(`Processed 10000 customers in ${duration}ms`);
    
    expect(duration).toBeLessThan(5000);
  }, 10000); // 10 second timeout
});
```

## Step 4.3: Create Test Data

### Tasks

1. **Extract production samples:**
   - Get representative data from production
   - Anonymize PII
   - Cover edge cases
   - Include error scenarios

2. **Create test fixtures:**
   ```
   test-data/
   ├── input/
   │   ├── customer-records.txt
   │   ├── transaction-file.dat
   │   └── edge-cases.txt
   ├── expected-output/
   │   ├── customer-output.txt
   │   └── transaction-output.txt
   └── cobol-output/
       ├── baseline-run-1.txt
       └── baseline-run-2.txt
   ```

3. **Generate test data:**
   ```java
   public class TestDataGenerator {
       public static List<Customer> generateCustomers(int count) {
           List<Customer> customers = new ArrayList<>();
           for (int i = 0; i < count; i++) {
               customers.add(Customer.builder()
                   .customerId(String.format("CUST%06d", i))
                   .customerName("Test Customer " + i)
                   .balance(new BigDecimal("1000.00"))
                   .accountStatus("A")
                   .build());
           }
           return customers;
       }
   }
   ```

4. **Edge cases to test:**
   - Maximum field lengths
   - Minimum/maximum numeric values
   - Zero values
   - Negative numbers
   - Empty strings
   - Special characters
   - Date boundaries (leap years, century changes)
   - Decimal precision limits
   - Array/table boundaries (OCCURS)

## Step 4.4: Validation Testing

### Comparison Test Framework

Create automated comparison tool:

```java
public class MigrationValidator {
    public ValidationReport compareOutputs(
        Path cobolOutput,
        Path migratedOutput
    ) {
        List<String> cobolLines = Files.readAllLines(cobolOutput);
        List<String> migratedLines = Files.readAllLines(migratedOutput);
        
        ValidationReport report = new ValidationReport();
        
        if (cobolLines.size() != migratedLines.size()) {
            report.addError("Line count mismatch");
        }
        
        for (int i = 0; i < Math.min(cobolLines.size(), migratedLines.size()); i++) {
            if (!cobolLines.get(i).equals(migratedLines.get(i))) {
                report.addDifference(i + 1, cobolLines.get(i), migratedLines.get(i));
            }
        }
        
        return report;
    }
}
```

### Numeric Precision Validation

```java
@Test
void validateNumericPrecision() {
    // Test that BigDecimal maintains COBOL precision
    BigDecimal cobolValue = new BigDecimal("123.45");
    BigDecimal result = service.calculate(cobolValue);
    
    // Verify scale is preserved
    assertEquals(2, result.scale());
    
    // Verify exact value
    assertEquals(0, expectedValue.compareTo(result));
}
```

### Data Integrity Tests

```java
@Test
void validateDataIntegrity() {
    // Process records
    List<Customer> input = loadTestData();
    List<Customer> output = service.processAll(input);
    
    // Verify no data loss
    assertEquals(input.size(), output.size());
    
    // Verify checksums match
    BigDecimal inputSum = calculateChecksum(input);
    BigDecimal outputSum = calculateChecksum(output);
    assertEquals(0, inputSum.compareTo(outputSum));
}
```

## Step 4.5: Test Documentation

### Create test report template:

```markdown
# Migration Test Report

## Test Summary
- Total Tests: X
- Passed: Y
- Failed: Z
- Coverage: XX%

## Unit Test Results
- Service Layer: PASS
- Repository Layer: PASS
- Utility Classes: PASS

## Integration Test Results
- End-to-End Workflows: PASS
- Database Operations: PASS
- File I/O: PASS

## Comparison Test Results
- Output Matching: PASS
- Numeric Precision: PASS
- Data Integrity: PASS

## Performance Test Results
- Throughput: X records/second
- Memory Usage: X MB
- Response Time: X ms

## Issues Found
1. [Issue description]
2. [Issue description]

## Recommendations
1. [Recommendation]
2. [Recommendation]
```

## Validation Checklist

Before completing Phase 4:
- [ ] All unit tests created and passing
- [ ] Integration tests created and passing
- [ ] Comparison tests validate output matches COBOL
- [ ] Performance tests show acceptable performance
- [ ] Test coverage > 80%
- [ ] Edge cases tested
- [ ] Test data created and documented
- [ ] Test report generated
- [ ] All critical issues resolved

## Next Phase

Proceed to Phase 5 (Migration Utilities & Tools)
