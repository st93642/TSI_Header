const assert = require('assert');
const path = require('path');    // const resolvedAfterFinal = learnManager.getNextLessonForSolution(curriculum, progress, 'polymorphism_cpp');
    // assert(resolvedAfterFinal, 'Expected a fallback lesson after reaching the end');
    // assert.strictEqual(resolvedAfterFinal.id, nextFromProgress.id, 'At curriculum end, fall back to first incomplete lesson');nst LearnManager = require('../lib/learn_manager');

const contextStub = {
    subscriptions: [],
    workspaceState: {
        update: async () => {}
    },
    globalState: {
        get: () => ({}),
        update: async () => {}
    }
};

const vscodeStub = {
    window: {
        tabGroups: {
            all: []
        }
    }
};

function loadCurriculum(language) {
    const curriculumPath = path.join(__dirname, '..', 'curriculum', language, 'curriculum.json');
    return require(curriculumPath);
}

(async () => {
    const learnManager = new LearnManager(contextStub, vscodeStub, {});
    const curriculum = loadCurriculum('cpp');

    // Ensure we can walk sequentially from lesson 1.2 to 2.1 when current lesson isn't marked complete.
    const progress = {
        completed: ['hello_world_cpp']
    };

    const nextFromProgress = learnManager.getNextLesson(curriculum, progress);
    assert.strictEqual(nextFromProgress.id, 'iostream_basics');

    const resolvedNext112 = learnManager.getNextLessonForSolution(curriculum, progress, 'iostream_basics');
    assert(resolvedNext112, 'Expected a next lesson for solution navigation');
    assert.strictEqual(resolvedNext112.id, 'variables_types_cpp', 'Should advance sequentially even if earlier lessons are incomplete');

    const resolvedBeyondModule = learnManager.getNextLessonForSolution(curriculum, progress, 'variables_types_cpp');
    assert(resolvedBeyondModule, 'Expected next lesson after variables module');
    assert.strictEqual(resolvedBeyondModule.id, 'arithmetic_input_cpp', 'Should continue to next chronological lesson');

    const sequentialAfterModuleFour = learnManager.getLessonAfter(curriculum, 'header_basics_cpp');
    assert(sequentialAfterModuleFour, 'Lesson after Module 4 should exist');
    assert.strictEqual(sequentialAfterModuleFour.id, 'vectors_cpp', 'Should advance to Module 5 after headers');

    const sequentialAfterModuleFive = learnManager.getLessonAfter(curriculum, 'structs_cpp');
    assert(sequentialAfterModuleFive, 'Lesson after Module 5 should exist');
    assert.strictEqual(sequentialAfterModuleFive.id, 'classes_objects_cpp', 'Should advance to classes lesson after structs');

    const sequentialAfterModuleSix = learnManager.getLessonAfter(curriculum, 'stl_algorithms_cpp');
    assert(sequentialAfterModuleSix, 'Lesson after Module 6 should exist');
    assert.strictEqual(sequentialAfterModuleSix.id, 'classes_encapsulation_cpp', 'Should advance to OOP module after algorithms');

    const sequentialAfterOOP = learnManager.getLessonAfter(curriculum, 'polymorphism_cpp');
    assert.strictEqual(sequentialAfterOOP, null, 'Final lesson should not return a next lesson');

    const resolvedAfterFinal = learnManager.getNextLessonForSolution(curriculum, progress, 'stl_algorithms_cpp');
    assert(resolvedAfterFinal, 'Expected a fallback lesson after reaching the end');
    assert.strictEqual(resolvedAfterFinal.id, nextFromProgress.id, 'At curriculum end, fall back to first incomplete lesson');

    console.log('All solution navigation tests passed.');
    process.exit(0);
})().catch(error => {
    console.error(error);
    process.exit(1);
});
