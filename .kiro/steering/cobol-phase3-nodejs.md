---
inclusion: manual
---

# Phase 3: COBOL to Node.js Migration

You are migrating COBOL code to Node.js/TypeScript for cloud deployment.

## Prerequisites

- Phase 1 analysis completed
- Data structure mappings available
- Business logic documented

## Step 3.1: Node.js Project Setup

### Tasks

1. **Initialize Node.js project structure:**
   ```
   project-root/
   ├── src/
   │   ├── models/        (TypeScript types/interfaces)
   │   ├── services/      (business logic)
   │   ├── repositories/  (data access)
   │   ├── utils/         (utilities)
   │   ├── legacy/        (COBOL compatibility)
   │   └── index.ts       (entry point)
   ├── tests/
   │   ├── unit/
   │   └── integration/
   ├── package.json
   ├── tsconfig.json
   ├── .env.example
   ├── .eslintrc.json
   ├── .prettierrc
   └── README.md
   ```

2. **Initialize with npm/yarn:**
   ```bash
   npm init -y
   npm install typescript @types/node ts-node
   npm install express @types/express  # if creating APIs
   npm install dotenv
   npm install winston  # logging
   npm install joi class-validator  # validation
   npm install decimal.js  # precision arithmetic
   ```

3. **Install database dependencies:**
   - PostgreSQL: `pg @types/pg` or `sequelize`
   - MySQL: `mysql2` or `typeorm`
   - MongoDB: `mongodb` or `mongoose`

4. **Install dev dependencies:**
   ```bash
   npm install -D jest @types/jest ts-jest
   npm install -D eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin
   npm install -D prettier
   npm install -D nodemon
   ```

5. **Configure tsconfig.json:**
   ```json
   {
     "compilerOptions": {
       "target": "ES2022",
       "module": "commonjs",
       "lib": ["ES2022"],
       "outDir": "./dist",
       "rootDir": "./src",
       "strict": true,
       "esModuleInterop": true,
       "skipLibCheck": true,
       "forceConsistentCasingInFileNames": true,
       "resolveJsonModule": true,
       "declaration": true,
       "declarationMap": true,
       "sourceMap": true
     },
     "include": ["src/**/*"],
     "exclude": ["node_modules", "dist", "tests"]
   }
   ```

6. **Configure package.json scripts:**
   ```json
   {
     "scripts": {
       "build": "tsc",
       "start": "node dist/index.js",
       "dev": "nodemon --exec ts-node src/index.ts",
       "test": "jest",
       "test:watch": "jest --watch",
       "lint": "eslint src/**/*.ts",
       "format": "prettier --write src/**/*.ts"
     }
   }
   ```

### Output

- Complete Node.js/TypeScript project structure
- package.json with all dependencies
- Configuration files (tsconfig, eslint, prettier)
- README with setup instructions

## Step 3.2: Convert Data Structures to TypeScript

### Tasks

1. **Create TypeScript interfaces for COBOL records:**
   ```typescript
   /**
    * Customer record from COBOL CUSTOMER-RECORD
    * Original: CUSTOMER-MAINT.cbl lines 100-150
    */
   export interface CustomerRecord {
     customerId: string;        // PIC X(10)
     customerName: string;      // PIC X(50)
     balance: Decimal;          // PIC 9(9)V99 COMP-3
     accountStatus: string;     // PIC X(1)
     lastUpdated: Date;         // PIC 9(8) (YYYYMMDD)
   }
   ```

2. **Create classes for complex structures:**
   ```typescript
   export class Customer {
     constructor(
       public customerId: string,
       public customerName: string,
       public balance: Decimal,
       public accountStatus: string,
       public lastUpdated: Date
     ) {}

     // Validation methods
     isActive(): boolean {
       return this.accountStatus === 'A';
     }

     // COBOL-style operations
     addToBalance(amount: Decimal): void {
       this.balance = this.balance.plus(amount);
     }
   }
   ```

3. **Handle COBOL-specific features:**
   - OCCURS → Array<Type>
   - REDEFINES → Union types or separate interfaces
   - COMP-3 → Decimal from decimal.js
   - Level 88 → enum or type literals
   - Group items → nested interfaces

4. **Create CobolDataConverter utility:**
   ```typescript
   import { Decimal } from 'decimal.js';

   export class CobolDataConverter {
     // Packed decimal conversion
     static unpackComp3(buffer: Buffer): Decimal {
       // Implementation
     }

     static packComp3(value: Decimal): Buffer {
       // Implementation
     }

     // Date conversions
     static cobolDateToJs(cobolDate: string, format: string): Date {
       // Implementation
     }

     static jsDateToCobol(date: Date, format: string): string {
       // Implementation
     }

     // String operations
     static padRight(str: string, length: number, padChar: string = ' '): string {
       return str.padEnd(length, padChar);
     }

     static padLeft(str: string, length: number, padChar: string = '0'): string {
       return str.padStart(length, padChar);
     }

     // EBCDIC conversion
     static ebcdicToAscii(buffer: Buffer): string {
       // Implementation using iconv-lite
     }
   }
   ```

5. **Create barrel exports:**
   ```typescript
   // src/models/index.ts
   export * from './customer';
   export * from './account';
   export * from './transaction';
   ```

### Output

- TypeScript interfaces for all COBOL data structures
- Class implementations with validation
- CobolDataConverter utility module
- Unit tests for conversions
- Type documentation

## Step 3.3: Convert Program Logic to Node.js

### Tasks

1. **Create service class for each COBOL program:**
   ```typescript
   import { Decimal } from 'decimal.js';
   import { CustomerRecord } from '../models';
   import { logger } from '../utils/logger';

   export class CustomerMaintenanceService {
     /**
      * Process customer maintenance
      * Migrated from CUSTOMER-MAINT.cbl
      */
     async processCustomer(customerId: string): Promise<CustomerRecord> {
       try {
         logger.info(`Processing customer: ${customerId}`);
         // Business logic here
       } catch (error) {
         logger.error(`Error processing customer: ${error}`);
         throw error;
       }
     }
   }
   ```

2. **Convert COBOL control structures:**
   - PERFORM UNTIL → while loops or async iteration
   - PERFORM VARYING → for loops or array methods
   - EVALUATE → switch statements
   - IF-ELSE → if-else or ternary
   - COMPUTE → Decimal arithmetic
   - GO TO → refactor to structured code

3. **Use async/await patterns:**
   ```typescript
   async function processRecords(records: CustomerRecord[]): Promise<void> {
     for (const record of records) {
       await processRecord(record);
     }
   }
   ```

4. **Preserve COBOL behavior:**
   - Use Decimal.js for all financial calculations
   - Configure Decimal rounding to match COBOL
   - Preserve truncation behavior
   - Maintain validation rules
   - Keep error handling logic

5. **Modern TypeScript features:**
   - Destructuring: `const { customerId, balance } = customer;`
   - Arrow functions: `records.map(r => r.customerId)`
   - Optional chaining: `customer?.address?.city`
   - Nullish coalescing: `value ?? defaultValue`
   - Template literals: `` `Customer ${id}` ``

6. **Error handling:**
   ```typescript
   try {
     await processCustomer(id);
   } catch (error) {
     if (error instanceof ValidationError) {
       logger.warn(`Validation failed: ${error.message}`);
     } else {
       logger.error(`Unexpected error: ${error}`);
       throw error;
     }
   }
   ```

### Output

- Service classes for each COBOL program
- Async/await implementations
- Unit tests with Jest
- Integration tests
- JSDoc documentation

## Step 3.4: Convert File I/O to Node.js

### Tasks

1. **For sequential files:**
   ```typescript
   import { createReadStream, createWriteStream } from 'fs';
   import { promises as fs } from 'fs';
   import readline from 'readline';

   export class CobolFileHandler {
     async readSequentialFile(filePath: string): Promise<CustomerRecord[]> {
       const records: CustomerRecord[] = [];
       const fileStream = createReadStream(filePath);
       const rl = readline.createInterface({
         input: fileStream,
         crlfDelay: Infinity
       });

       for await (const line of rl) {
         const record = this.parseFixedLengthRecord(line);
         records.push(record);
       }

       return records;
     }

     parseFixedLengthRecord(line: string): CustomerRecord {
       // Parse fixed-length fields
       const customerId = line.substring(0, 10).trim();
       const customerName = line.substring(10, 60).trim();
       // ... more fields
       return { customerId, customerName, /* ... */ };
     }
   }
   ```

2. **For indexed files (VSAM → Database):**
   - Design database schema
   - Use ORM (Sequelize, TypeORM, or Prisma)
   - Create models with decorators
   - Implement key-based access

3. **Convert COBOL file operations:**
   - OPEN → file handle or connection initialization
   - READ → async read with EOF handling
   - WRITE → async write operations
   - REWRITE → update operations
   - DELETE → delete operations
   - CLOSE → proper cleanup (finally blocks)

4. **Stream processing for large files:**
   ```typescript
   import { Transform } from 'stream';

   const processStream = new Transform({
     objectMode: true,
     transform(chunk, encoding, callback) {
       const record = parseRecord(chunk.toString());
       const processed = processRecord(record);
       callback(null, processed);
     }
   });

   createReadStream('input.dat')
     .pipe(processStream)
     .pipe(createWriteStream('output.dat'));
   ```

5. **Error handling:**
   - File not found → catch ENOENT
   - I/O errors → catch and log
   - Data format errors → custom ParseError
   - EOF → natural end of iteration

### Output

- File handler classes
- Stream processing utilities
- Database models for indexed files
- Unit tests with sample files
- Integration tests

## Step 3.5: Convert Database Operations to Node.js

### Tasks

1. **Choose ORM/Query Builder:**
   - Sequelize (SQL, mature)
   - TypeORM (TypeScript-first)
   - Prisma (modern, type-safe)
   - Knex.js (query builder)

2. **Create models (TypeORM example):**
   ```typescript
   import { Entity, PrimaryGeneratedColumn, Column } from 'typeorm';
   import { Decimal } from 'decimal.js';

   @Entity('customers')
   export class Customer {
     @PrimaryGeneratedColumn()
     id: number;

     @Column({ length: 10, unique: true })
     customerId: string;

     @Column({ length: 50 })
     customerName: string;

     @Column('decimal', { precision: 11, scale: 2 })
     balance: Decimal;

     @Column({ length: 1 })
     accountStatus: string;

     @Column('timestamp')
     lastUpdated: Date;
   }
   ```

3. **Create repository pattern:**
   ```typescript
   export class CustomerRepository {
     constructor(private dataSource: DataSource) {}

     async findByCustomerId(customerId: string): Promise<Customer | null> {
       return await this.dataSource
         .getRepository(Customer)
         .findOne({ where: { customerId } });
     }

     async findHighBalanceCustomers(amount: Decimal): Promise<Customer[]> {
       return await this.dataSource
         .getRepository(Customer)
         .createQueryBuilder('customer')
         .where('customer.balance > :amount', { amount: amount.toString() })
         .getMany();
     }

     async save(customer: Customer): Promise<Customer> {
       return await this.dataSource.getRepository(Customer).save(customer);
     }
   }
   ```

4. **Convert SQL operations:**
   - SELECT single → findOne()
   - SELECT multiple → find() or query builder
   - INSERT → save(newEntity)
   - UPDATE → load, modify, save()
   - DELETE → remove() or delete()
   - Cursors → async generators or pagination

5. **Handle COBOL SQL patterns:**
   - Host variables → function parameters
   - SQLCODE checks → try-catch error handling
   - Multi-row fetches → arrays or async iteration
   - Transactions → transaction manager

6. **Transaction handling:**
   ```typescript
   async function transferFunds(fromId: string, toId: string, amount: Decimal) {
     await dataSource.transaction(async (manager) => {
       const fromAccount = await manager.findOne(Customer, { where: { customerId: fromId } });
       const toAccount = await manager.findOne(Customer, { where: { customerId: toId } });
       
       fromAccount.balance = fromAccount.balance.minus(amount);
       toAccount.balance = toAccount.balance.plus(amount);
       
       await manager.save([fromAccount, toAccount]);
     });
   }
   ```

7. **Create migrations:**
   - Use ORM migration tools
   - Version control schema changes
   - Include seed data

### Output

- Database models/entities
- Repository classes
- Migration scripts
- Database configuration
- Integration tests with test database
- Connection pooling setup

## Validation Checklist

Before completing Phase 3:
- [ ] Node.js project builds successfully
- [ ] All TypeScript compiles without errors
- [ ] All data structures converted
- [ ] All business logic migrated
- [ ] File I/O operations implemented
- [ ] Database operations implemented
- [ ] Unit tests passing
- [ ] Integration tests passing
- [ ] Code follows TypeScript best practices
- [ ] Documentation complete

## Next Phase

Proceed to Phase 4 (Testing & Validation)
