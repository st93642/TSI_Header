/**
 * Progress Tracker - Manages learning progress and statistics
 */

class ProgressTracker {
    constructor(context) {
        this.context = context;
    }

    /**
     * Get progress for a language
     * @param {string} language - Programming language
     * @returns {Promise<Object>} Progress object
     */
    async getProgress(language) {
        const progressKey = `learn_progress_${language}`;
        const defaultProgress = {
            completed: [],
            exercisesCompleted: [],
            streakDays: 0,
            lastStudyDate: null,
            totalTimeMinutes: 0,
            achievements: []
        };
        
        const stored = this.context.globalState.get(progressKey);
        
        // If nothing stored, return defaults
        if (!stored) {
            return defaultProgress;
        }
        
        // Merge stored data with defaults to ensure all properties exist
        return {
            completed: stored.completed || [],
            exercisesCompleted: stored.exercisesCompleted || [],
            streakDays: stored.streakDays || 0,
            lastStudyDate: stored.lastStudyDate || null,
            totalTimeMinutes: stored.totalTimeMinutes || 0,
            achievements: stored.achievements || []
        };
    }

    /**
     * Record completion of an exercise
     * @param {string} language - Programming language
     * @param {string} exerciseId - Exercise ID
     */
    async recordCompletion(language, exerciseId) {
        const progress = await this.getProgress(language);
        
        if (!progress.exercisesCompleted.includes(exerciseId)) {
            progress.exercisesCompleted.push(exerciseId);
        }
        
        // Extract lesson ID from exercise ID (format: lessonId_exercise)
        const lessonId = exerciseId.replace(/_exercise$/, '');
        
        // Also mark the lesson as complete
        if (!progress.completed.includes(lessonId)) {
            progress.completed.push(lessonId);
        }
        
        // Update streak
        await this.updateStreak(progress);
        
        // Check for achievements
        await this.checkAchievements(language, progress);
        
        // Save progress
        const progressKey = `learn_progress_${language}`;
        await this.context.globalState.update(progressKey, progress);
    }

    /**
     * Update study streak
     * @param {Object} progress - Progress object
     */
    async updateStreak(progress) {
        const today = new Date().toDateString();
        const lastStudy = progress.lastStudyDate;
        
        if (lastStudy) {
            const lastDate = new Date(lastStudy);
            const daysDiff = Math.floor((new Date(today) - lastDate) / (1000 * 60 * 60 * 24));
            
            if (daysDiff === 0) {
                // Same day, no change
                return;
            } else if (daysDiff === 1) {
                // Consecutive day, increment streak
                progress.streakDays++;
            } else {
                // Streak broken
                progress.streakDays = 1;
            }
        } else {
            // First study day
            progress.streakDays = 1;
        }
        
        progress.lastStudyDate = today;
    }

    /**
     * Check and award achievements
     * @param {string} language - Programming language
     * @param {Object} progress - Progress object
     */
    async checkAchievements(language, progress) {
        const achievements = [
            {
                id: 'first_exercise',
                title: '🎯 First Steps',
                description: 'Complete your first exercise',
                condition: () => progress.exercisesCompleted.length >= 1
            },
            {
                id: 'five_exercises',
                title: '🔥 Getting Started',
                description: 'Complete 5 exercises',
                condition: () => progress.exercisesCompleted.length >= 5
            },
            {
                id: 'ten_exercises',
                title: '⭐ Dedicated Learner',
                description: 'Complete 10 exercises',
                condition: () => progress.exercisesCompleted.length >= 10
            },
            {
                id: 'streak_3',
                title: '📅 Three Day Streak',
                description: 'Study for 3 consecutive days',
                condition: () => progress.streakDays >= 3
            },
            {
                id: 'streak_7',
                title: '🏆 Week Warrior',
                description: 'Study for 7 consecutive days',
                condition: () => progress.streakDays >= 7
            },
            {
                id: 'streak_30',
                title: '💎 Monthly Master',
                description: 'Study for 30 consecutive days',
                condition: () => progress.streakDays >= 30
            }
        ];
        
        for (const achievement of achievements) {
            if (!progress.achievements.includes(achievement.id) && achievement.condition()) {
                progress.achievements.push(achievement.id);
                
                // Show achievement notification
                const vscode = require('vscode');
                vscode.window.showInformationMessage(
                    `🎉 Achievement Unlocked: ${achievement.title}\n${achievement.description}`,
                    { modal: true },
                    'Got it!'
                );
            }
        }
    }

    /**
     * Get statistics for a language
     * @param {string} language - Programming language
     * @returns {Promise<Object>} Statistics object
     */
    async getStats(language) {
        const progress = await this.getProgress(language);
        
        // Defensive checks to ensure all properties exist
        const completed = progress.completed || [];
        const exercisesCompleted = progress.exercisesCompleted || [];
        const achievements = progress.achievements || [];
        
        return {
            lessonsCompleted: completed.length,
            exercisesCompleted: exercisesCompleted.length,
            currentStreak: progress.streakDays || 0,
            totalStudyTime: progress.totalTimeMinutes || 0,
            achievements: achievements.length,
            lastStudyDate: progress.lastStudyDate || 'Never'
        };
    }

    /**
     * Record study time
     * @param {string} language - Programming language
     * @param {number} minutes - Minutes studied
     */
    async recordStudyTime(language, minutes) {
        const progress = await this.getProgress(language);
        progress.totalTimeMinutes += minutes;
        
        const progressKey = `learn_progress_${language}`;
        await this.context.globalState.update(progressKey, progress);
    }

    /**
     * Reset progress for a language
     * @param {string} language - Programming language
     */
    async resetProgress(language) {
        const progressKey = `learn_progress_${language}`;
        await this.context.globalState.update(progressKey, undefined);
    }

    /**
     * Get leaderboard data (for future multiplayer features)
     * @returns {Promise<Array>} Leaderboard entries
     */
    async getLeaderboard() {
        // Placeholder for future implementation
        return [];
    }
}

module.exports = ProgressTracker;
