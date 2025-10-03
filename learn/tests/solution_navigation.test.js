const assert = require('assert');
const path = require('path');
const LearnManager = require('../lib/learn_manager');

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

    const expectedSequence = [
        'hello_world_cpp',
        'iostream_basics',
        'variables_types_cpp',
        'arithmetic_input_cpp',
        'conditionals_cpp',
        'loops_cpp',
        'functions_cpp',
        'header_basics_cpp',
        'vectors_cpp',
        'structs_cpp',
        'classes_objects_cpp',
        'maps_cpp',
        'stl_algorithms_cpp',
        'classes_encapsulation_cpp',
        'inheritance_cpp',
        'polymorphism_cpp',
        'function_class_templates_cpp',
        'stl_internals_cpp',
        'basic_exceptions_cpp',
        'exception_safety_cpp',
        'file_streams_cpp',
    'priority_queues_intro_cpp',
    'priority_queues_heaps_cpp',
    'union_find_disjoint_sets_cpp'
    ];

    for (let index = 0; index < expectedSequence.length - 1; index++) {
        const currentLessonId = expectedSequence[index];
        const expectedNextId = expectedSequence[index + 1];

        const sequentialNext = learnManager.getLessonAfter(curriculum, currentLessonId);
        assert(sequentialNext, `Lesson after ${currentLessonId} should exist`);
        assert.strictEqual(sequentialNext.id, expectedNextId, `Expected ${expectedNextId} after ${currentLessonId}`);

        const solutionNavigationNext = learnManager.getNextLessonForSolution(curriculum, progress, currentLessonId);
        assert(solutionNavigationNext, `Solution navigation should provide a lesson after ${currentLessonId}`);
        assert.strictEqual(solutionNavigationNext.id, expectedNextId, `Solution navigation should advance from ${currentLessonId} to ${expectedNextId}`);
    }

    const finalLessonId = expectedSequence[expectedSequence.length - 1];
    const sequentialAfterFinal = learnManager.getLessonAfter(curriculum, finalLessonId);
    assert.strictEqual(sequentialAfterFinal, null, 'Final lesson should not return a next lesson');

    const resolvedAfterFinal = learnManager.getNextLessonForSolution(curriculum, progress, finalLessonId);
    assert(resolvedAfterFinal, 'Expected a fallback lesson after reaching the end');
    assert.strictEqual(resolvedAfterFinal.id, nextFromProgress.id, 'At curriculum end, fall back to first incomplete lesson');

    console.log('All solution navigation tests passed.');
    process.exit(0);
})().catch(error => {
    console.error(error);
    process.exit(1);
});
