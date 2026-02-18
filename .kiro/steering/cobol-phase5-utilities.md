---
inclusion: manual
---

# Phase 5: Migration Utilities & Tools

You are creating utility libraries and tools to support the COBOL migration.

## Objectives

1. Create COBOL compatibility layers
2. Build data conversion utilities
3. Create validation and comparison tools
4. Provide reusable migration components

## Step 5.1: Java Compatibility Layer

### CobolDataTypes Utility

```java
package com.migration.cobol.legacy;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.ByteBuffer;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Utility class for COBOL data type conversions and operations.
 * Replicates COBOL behavior for data handling.
 */
public class CobolDataTypes {
    
    /**
     * Unpack COMP-3 (packed decimal) bytes to BigDecimal
     * COBOL COMP-3 format: each byte contains 2 digits, last nibble is sign
     */
    public static BigDecimal unpackComp3(byte[] packed, int scale) {
        if (packed == null || packed.length == 0) {
            return BigDecimal.ZERO;
        }
        
        StringBuilder digits = new StringBuilder();
        boolean isNegative = false;
        
        for (int i = 0; i < packed.length; i++) {
            int b = packed[i] & 0xFF;
            int highNibble = (b >> 4) & 0x0F;
            int lowNibble = b & 0x0F;
            
            if (i == packed.length - 1) {
                // Last byte: high nibble is digit, low nibble is sign
                digits.append(highNibble);
                isNegative = (lowNibble == 0x0D); // D = negative
            } else {
                digits.append(highNibble);
                digits.append(lowNibble);
            }
        }
        
        BigDecimal value = new BigDecimal(digits.toString());
        value = value.movePointLeft(scale);
        
        return isNegative ? value.negate() : value;
    }
    
    /**
     * Pack BigDecimal to COMP-3 format
     */
    public static byte[] packComp3(BigDecimal value, int totalDigits, int scale) {
        boolean isNegative = value.signum() < 0;
        BigDecimal absValue = value.abs();
        
        // Scale to integer
        BigDecimal scaled = absValue.movePointRight(scale);
        String digits = String.format("%0" + totalDigits + "d", scaled.longValue());
        
        // Calculate packed length
        int packedLength = (totalDigits + 2) / 2;
        byte[] packed = new byte[packedLength];
        
        int digitIndex = 0;
        for (int i = 0; i < packedLength - 1; i++) {
            int high = Character.digit(digits.charAt(digitIndex++), 10);
            int low = Character.digit(digits.charAt(digitIndex++), 10);
            packed[i] = (byte) ((high << 4) | low);
        }
        
        // Last byte
        int lastDigit = Character.digit(digits.charAt(digitIndex), 10);
        int sign = isNegative ? 0x0D : 0x0C; // C = positive, D = negative
        packed[packedLength - 1] = (byte) ((lastDigit << 4) | sign);
        
        return packed;
    }
    
    /**
     * Pad string to right with spaces (PIC X behavior)
     */
    public static String padRight(String str, int length) {
        if (str == null) str = "";
        if (str.length() >= length) {
            return str.substring(0, length);
        }
        return String.format("%-" + length + "s", str);
    }
    
    /**
     * Pad numeric string to left with zeros (PIC 9 behavior)
     */
    public static String padLeft(String str, int length, char padChar) {
        if (str == null) str = "";
        if (str.length() >= length) {
            return str.substring(0, length);
        }
        return String.format("%" + padChar + length + "s", str).replace(' ', padChar);
    }
    
    /**
     * Convert COBOL date format to LocalDate
     * Supports: YYYYMMDD, YYMMDD, MMDDYYYY, etc.
     */
    public static LocalDate cobolDateToJava(String cobolDate, String format) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(format);
        return LocalDate.parse(cobolDate, formatter);
    }
    
    /**
     * Convert LocalDate to COBOL date format
     */
    public static String javaDateToCobol(LocalDate date, String format) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(format);
        return date.format(formatter);
    }
    
    /**
     * EBCDIC to ASCII conversion
     */
    public static String ebcdicToAscii(byte[] ebcdic) {
        // Use IBM037 charset for EBCDIC
        try {
            return new String(ebcdic, "IBM037");
        } catch (Exception e) {
            throw new RuntimeException("EBCDIC conversion failed", e);
        }
    }
    
    /**
     * ASCII to EBCDIC conversion
     */
    public static byte[] asciiToEbcdic(String ascii) {
        try {
            return ascii.getBytes("IBM037");
        } catch (Exception e) {
            throw new RuntimeException("EBCDIC conversion failed", e);
        }
    }
}
```

### CobolBehavior Utility

```java
package com.migration.cobol.legacy;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Replicates COBOL arithmetic and comparison behaviors
 */
public class CobolBehavior {
    
    /**
     * COBOL ROUNDED arithmetic
     * Rounds to nearest, ties away from zero
     */
    public static BigDecimal roundCobol(BigDecimal value, int scale) {
        return value.setScale(scale, RoundingMode.HALF_UP);
    }
    
    /**
     * COBOL truncation (default behavior without ROUNDED)
     */
    public static BigDecimal truncateCobol(BigDecimal value, int scale) {
        return value.setScale(scale, RoundingMode.DOWN);
    }
    
    /**
     * COBOL COMPUTE with size error checking
     */
    public static BigDecimal computeWithSizeCheck(
        BigDecimal result,
        int maxDigits,
        int scale
    ) throws ArithmeticException {
        BigDecimal maxValue = new BigDecimal("9".repeat(maxDigits - scale) + "." + "9".repeat(scale));
        
        if (result.abs().compareTo(maxValue) > 0) {
            throw new ArithmeticException("Size error: value exceeds PIC specification");
        }
        
        return result.setScale(scale, RoundingMode.DOWN);
    }
    
    /**
     * COBOL string comparison (space-padded)
     */
    public static int compareCobolStrings(String s1, String s2, int length) {
        String padded1 = CobolDataTypes.padRight(s1, length);
        String padded2 = CobolDataTypes.padRight(s2, length);
        return padded1.compareTo(padded2);
    }
    
    /**
     * COBOL MOVE with truncation
     */
    public static String moveString(String source, int targetLength) {
        if (source == null) source = "";
        if (source.length() > targetLength) {
            return source.substring(0, targetLength);
        }
        return CobolDataTypes.padRight(source, targetLength);
    }
    
    /**
     * COBOL numeric MOVE with truncation
     */
    public static BigDecimal moveNumeric(BigDecimal source, int digits, int scale) {
        return truncateCobol(source, scale);
    }
}
```

### CobolFileHandler

```java
package com.migration.cobol.legacy;

import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * Handles COBOL-style file operations
 */
public class CobolFileHandler {
    
    /**
     * Read fixed-length records from file
     */
    public static List<String> readFixedLengthFile(Path filePath, int recordLength) throws IOException {
        List<String> records = new ArrayList<>();
        
        try (BufferedInputStream bis = new BufferedInputStream(Files.newInputStream(filePath))) {
            byte[] buffer = new byte[recordLength];
            int bytesRead;
            
            while ((bytesRead = bis.read(buffer)) != -1) {
                if (bytesRead == recordLength) {
                    records.add(new String(buffer));
                } else {
                    // Partial record at end of file
                    records.add(new String(buffer, 0, bytesRead));
                }
            }
        }
        
        return records;
    }
    
    /**
     * Write fixed-length records to file
     */
    public static void writeFixedLengthFile(Path filePath, List<String> records, int recordLength) throws IOException {
        try (BufferedOutputStream bos = new BufferedOutputStream(Files.newOutputStream(filePath))) {
            for (String record : records) {
                String padded = CobolDataTypes.padRight(record, recordLength);
                bos.write(padded.getBytes());
            }
        }
    }
    
    /**
     * Parse copybook layout and extract fields
     */
    public static Map<String, String> parseRecord(String record, Map<String, FieldDefinition> layout) {
        Map<String, String> fields = new HashMap<>();
        
        for (Map.Entry<String, FieldDefinition> entry : layout.entrySet()) {
            String fieldName = entry.getKey();
            FieldDefinition def = entry.getValue();
            
            String value = record.substring(def.start, def.start + def.length);
            fields.put(fieldName, value.trim());
        }
        
        return fields;
    }
    
    public static class FieldDefinition {
        public int start;
        public int length;
        public String type; // "X" or "9"
        
        public FieldDefinition(int start, int length, String type) {
            this.start = start;
            this.length = length;
            this.type = type;
        }
    }
}
```

## Step 5.2: Node.js Compatibility Layer

### cobolDataTypes.ts

```typescript
import { Decimal } from 'decimal.js';
import iconv from 'iconv-lite';

/**
 * COBOL data type conversions and operations
 */
export class CobolDataTypes {
  /**
   * Unpack COMP-3 (packed decimal) bytes to Decimal
   */
  static unpackComp3(buffer: Buffer, scale: number): Decimal {
    if (!buffer || buffer.length === 0) {
      return new Decimal(0);
    }

    let digits = '';
    let isNegative = false;

    for (let i = 0; i < buffer.length; i++) {
      const byte = buffer[i];
      const highNibble = (byte >> 4) & 0x0f;
      const lowNibble = byte & 0x0f;

      if (i === buffer.length - 1) {
        digits += highNibble.toString();
        isNegative = lowNibble === 0x0d;
      } else {
        digits += highNibble.toString() + lowNibble.toString();
      }
    }

    let value = new Decimal(digits);
    value = value.dividedBy(Math.pow(10, scale));

    return isNegative ? value.negated() : value;
  }

  /**
   * Pack Decimal to COMP-3 format
   */
  static packComp3(value: Decimal, totalDigits: number, scale: number): Buffer {
    const isNegative = value.isNegative();
    const absValue = value.abs();

    const scaled = absValue.times(Math.pow(10, scale));
    const digits = scaled.toFixed(0).padStart(totalDigits, '0');

    const packedLength = Math.ceil((totalDigits + 1) / 2);
    const buffer = Buffer.alloc(packedLength);

    let digitIndex = 0;
    for (let i = 0; i < packedLength - 1; i++) {
      const high = parseInt(digits[digitIndex++], 10);
      const low = parseInt(digits[digitIndex++], 10);
      buffer[i] = (high << 4) | low;
    }

    const lastDigit = parseInt(digits[digitIndex], 10);
    const sign = isNegative ? 0x0d : 0x0c;
    buffer[packedLength - 1] = (lastDigit << 4) | sign;

    return buffer;
  }

  /**
   * Pad string to right with spaces (PIC X behavior)
   */
  static padRight(str: string, length: number): string {
    if (!str) str = '';
    if (str.length >= length) {
      return str.substring(0, length);
    }
    return str.padEnd(length, ' ');
  }

  /**
   * Pad numeric string to left with zeros (PIC 9 behavior)
   */
  static padLeft(str: string, length: number, padChar: string = '0'): string {
    if (!str) str = '';
    if (str.length >= length) {
      return str.substring(0, length);
    }
    return str.padStart(length, padChar);
  }

  /**
   * Convert COBOL date to JavaScript Date
   */
  static cobolDateToJs(cobolDate: string, format: string): Date {
    // Simple implementation for common formats
    if (format === 'YYYYMMDD') {
      const year = parseInt(cobolDate.substring(0, 4), 10);
      const month = parseInt(cobolDate.substring(4, 6), 10) - 1;
      const day = parseInt(cobolDate.substring(6, 8), 10);
      return new Date(year, month, day);
    }
    throw new Error(`Unsupported date format: ${format}`);
  }

  /**
   * Convert JavaScript Date to COBOL format
   */
  static jsDateToCobol(date: Date, format: string): string {
    if (format === 'YYYYMMDD') {
      const year = date.getFullYear().toString();
      const month = (date.getMonth() + 1).toString().padStart(2, '0');
      const day = date.getDate().toString().padStart(2, '0');
      return year + month + day;
    }
    throw new Error(`Unsupported date format: ${format}`);
  }

  /**
   * EBCDIC to ASCII conversion
   */
  static ebcdicToAscii(buffer: Buffer): string {
    return iconv.decode(buffer, 'IBM037');
  }

  /**
   * ASCII to EBCDIC conversion
   */
  static asciiToEbcdic(str: string): Buffer {
    return iconv.encode(str, 'IBM037');
  }
}
```

### cobolBehavior.ts

```typescript
import { Decimal } from 'decimal.js';
import { CobolDataTypes } from './cobolDataTypes';

/**
 * Replicates COBOL arithmetic and comparison behaviors
 */
export class CobolBehavior {
  /**
   * COBOL ROUNDED arithmetic
   */
  static roundCobol(value: Decimal, scale: number): Decimal {
    return value.toDecimalPlaces(scale, Decimal.ROUND_HALF_UP);
  }

  /**
   * COBOL truncation
   */
  static truncateCobol(value: Decimal, scale: number): Decimal {
    return value.toDecimalPlaces(scale, Decimal.ROUND_DOWN);
  }

  /**
   * COBOL COMPUTE with size error checking
   */
  static computeWithSizeCheck(
    result: Decimal,
    maxDigits: number,
    scale: number
  ): Decimal {
    const maxValue = new Decimal('9'.repeat(maxDigits - scale) + '.' + '9'.repeat(scale));

    if (result.abs().greaterThan(maxValue)) {
      throw new Error('Size error: value exceeds PIC specification');
    }

    return this.truncateCobol(result, scale);
  }

  /**
   * COBOL string comparison
   */
  static compareCobolStrings(s1: string, s2: string, length: number): number {
    const padded1 = CobolDataTypes.padRight(s1, length);
    const padded2 = CobolDataTypes.padRight(s2, length);
    return padded1.localeCompare(padded2);
  }

  /**
   * COBOL MOVE with truncation
   */
  static moveString(source: string, targetLength: number): string {
    if (!source) source = '';
    if (source.length > targetLength) {
      return source.substring(0, targetLength);
    }
    return CobolDataTypes.padRight(source, targetLength);
  }

  /**
   * COBOL numeric MOVE
   */
  static moveNumeric(source: Decimal, digits: number, scale: number): Decimal {
    return this.truncateCobol(source, scale);
  }
}
```

### cobolFileHandler.ts

```typescript
import { createReadStream, createWriteStream } from 'fs';
import { promises as fs } from 'fs';
import readline from 'readline';

/**
 * Handles COBOL-style file operations
 */
export class CobolFileHandler {
  /**
   * Read fixed-length records from file
   */
  static async readFixedLengthFile(
    filePath: string,
    recordLength: number
  ): Promise<string[]> {
    const records: string[] = [];
    const buffer = await fs.readFile(filePath);

    for (let i = 0; i < buffer.length; i += recordLength) {
      const recordBuffer = buffer.slice(i, i + recordLength);
      records.push(recordBuffer.toString());
    }

    return records;
  }

  /**
   * Write fixed-length records to file
   */
  static async writeFixedLengthFile(
    filePath: string,
    records: string[],
    recordLength: number
  ): Promise<void> {
    const buffers = records.map((record) => {
      const padded = record.padEnd(recordLength, ' ');
      return Buffer.from(padded.substring(0, recordLength));
    });

    await fs.writeFile(filePath, Buffer.concat(buffers));
  }

  /**
   * Parse record using field layout
   */
  static parseRecord(
    record: string,
    layout: Map<string, FieldDefinition>
  ): Map<string, string> {
    const fields = new Map<string, string>();

    for (const [fieldName, def] of layout.entries()) {
      const value = record.substring(def.start, def.start + def.length);
      fields.set(fieldName, value.trim());
    }

    return fields;
  }
}

export interface FieldDefinition {
  start: number;
  length: number;
  type: 'X' | '9';
}
```

## Step 5.3: Migration Validation Tool

Create a command-line tool to validate migration:

```typescript
#!/usr/bin/env node

import { program } from 'commander';
import { readFile } from 'fs/promises';
import chalk from 'chalk';

interface ValidationResult {
  passed: boolean;
  totalLines: number;
  matchedLines: number;
  differences: Difference[];
}

interface Difference {
  lineNumber: number;
  cobolLine: string;
  migratedLine: string;
}

async function compareFiles(
  cobolFile: string,
  migratedFile: string
): Promise<ValidationResult> {
  const cobolContent = await readFile(cobolFile, 'utf-8');
  const migratedContent = await readFile(migratedFile, 'utf-8');

  const cobolLines = cobolContent.split('\n');
  const migratedLines = migratedContent.split('\n');

  const differences: Difference[] = [];
  let matchedLines = 0;

  const maxLines = Math.max(cobolLines.length, migratedLines.length);

  for (let i = 0; i < maxLines; i++) {
    const cobolLine = cobolLines[i] || '';
    const migratedLine = migratedLines[i] || '';

    if (cobolLine === migratedLine) {
      matchedLines++;
    } else {
      differences.push({
        lineNumber: i + 1,
        cobolLine,
        migratedLine,
      });
    }
  }

  return {
    passed: differences.length === 0,
    totalLines: maxLines,
    matchedLines,
    differences,
  };
}

program
  .name('cobol-validator')
  .description('Validate COBOL migration by comparing outputs')
  .version('1.0.0');

program
  .command('compare')
  .description('Compare COBOL output with migrated output')
  .argument('<cobol-file>', 'COBOL program output file')
  .argument('<migrated-file>', 'Migrated program output file')
  .action(async (cobolFile, migratedFile) => {
    console.log(chalk.blue('Comparing outputs...\n'));

    const result = await compareFiles(cobolFile, migratedFile);

    console.log(chalk.bold('Validation Results:'));
    console.log(`Total lines: ${result.totalLines}`);
    console.log(`Matched lines: ${result.matchedLines}`);
    console.log(`Differences: ${result.differences.length}\n`);

    if (result.passed) {
      console.log(chalk.green('✓ Validation PASSED - Outputs match exactly!'));
    } else {
      console.log(chalk.red('✗ Validation FAILED - Differences found:\n'));

      result.differences.slice(0, 10).forEach((diff) => {
        console.log(chalk.yellow(`Line ${diff.lineNumber}:`));
        console.log(chalk.gray('  COBOL:    ') + diff.cobolLine);
        console.log(chalk.gray('  Migrated: ') + diff.migratedLine);
        console.log('');
      });

      if (result.differences.length > 10) {
        console.log(chalk.gray(`... and ${result.differences.length - 10} more differences`));
      }
    }
  });

program.parse();
```

## Validation Checklist

Before completing Phase 5:
- [ ] Java compatibility layer created
- [ ] Node.js compatibility layer created
- [ ] All utility classes tested
- [ ] Validation tool created and tested
- [ ] Documentation for all utilities
- [ ] Usage examples provided
- [ ] Performance tested

## Next Phase

Proceed to Phase 6 (Documentation & Handoff)
