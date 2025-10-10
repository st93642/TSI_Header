const path = require('path');
const fs = require('fs').promises;
const { execSync } = require('child_process');

async function runSmoke() {
    const vscodeMock = {
        window: {
            activeTextEditor: null,
            showInformationMessage: () => {}
        }
    };
    const ExerciseRunner = require(path.join(__dirname, '..', 'learn', 'lib', 'exercise_runner.js'));
    const runner = new ExerciseRunner(vscodeMock);

    // Load sample exercise
    const exercisePath = path.join(__dirname, '..', 'learn', 'curriculum', 'rust', 'exercises', 'Chapter7_exercise.json');
    const exercise = JSON.parse(await fs.readFile(exercisePath, 'utf8'));

    // Create a temporary editor file for the runner to pick up
    const tempCode = `use std::collections::HashMap;

fn main() {
    let text = "hello world hello rust";
    
    // Vector to store words
    let mut words: Vec<&str> = Vec::new();
    for word in text.split_whitespace() {
        words.push(word);
    }
    
    println!("Words: {}", words.join(", "));
    println!("Word count: {}", words.len());
    
    // Count characters in the text
    let char_count = text.chars().count();
    println!("Character count: {}", char_count);
    
    // Hash map for word frequency counting
    let mut word_freq: HashMap<&str, u32> = HashMap::new();
    for word in &words {
        let count = word_freq.entry(word).or_insert(0);
        *count += 1;
    }
    
    // Find most frequent word
    let mut max_word = "";
    let mut max_count = 0;
    for (word, count) in &word_freq {
        if *count > max_count {
            max_word = word;
            max_count = *count;
        }
    }
    println!("Most frequent word: {} (appears {} times)", max_word, max_count);
    
    // Analyze characters - separate vowels and consonants
    let mut vowels: Vec<char> = Vec::new();
    let mut consonants: Vec<char> = Vec::new();
    
    for ch in text.chars() {
        if ch.is_alphabetic() {
            let lower_ch = ch.to_lowercase().next().unwrap();
            if matches!(lower_ch, 'a' | 'e' | 'i' | 'o' | 'u') {
                vowels.push(ch);
            } else {
                consonants.push(ch);
            }
        }
    }
    
    println!("Vowels: {}", vowels.iter().map(|c| c.to_string()).collect::<Vec<String>>().join(", "));
    println!("Consonants: {}", consonants.iter().map(|c| c.to_string()).collect::<Vec<String>>().join(", "));
    
    // Print word frequencies
    for (word, count) in &word_freq {
        println!("{}: {}", word, count);
    }
}`;
    const tempFile = path.join(__dirname, '..', '.temp_tests', 'solution.rs');
    await fs.mkdir(path.dirname(tempFile), { recursive: true });
    await fs.writeFile(tempFile, tempCode);

    // Create a fake activeTextEditor with document getting text returning tempCode
    vscodeMock.window.activeTextEditor = {
        document: {
            getText: () => tempCode
        }
    };

    try {
        const result = await runner.run('rust', exercise);
        console.log('Smoke test result:', result);
    } catch (e) {
        console.error('Smoke test error:', e);
        process.exit(1);
    }
}

runSmoke();
