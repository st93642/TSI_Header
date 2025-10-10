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
    const exercisePath = path.join(__dirname, '..', 'learn', 'curriculum', 'rust', 'exercises', 'Chapter6_exercise.json');
    const exercise = JSON.parse(await fs.readFile(exercisePath, 'utf8'));

    // Create a temporary editor file for the runner to pick up
    const tempCode = `mod library {
    pub mod music {
        #[derive(Debug)]
        pub struct Song {
            pub title: String,
            pub artist: String,
            pub duration: String,
        }

        impl Song {
            pub fn new(title: &str, artist: &str, duration: &str) -> Song {
                Song {
                    title: String::from(title),
                    artist: String::from(artist),
                    duration: String::from(duration),
                }
            }

            pub fn play(&self) {
                println!("Playing: {} by {}", self.title, self.artist);
            }
        }
    }

    pub mod playlists {
        use super::music::Song;

        #[derive(Debug)]
        pub struct Playlist {
            pub name: String,
            songs: Vec<Song>,
        }

        impl Playlist {
            pub fn new(name: &str) -> Playlist {
                Playlist {
                    name: String::from(name),
                    songs: Vec::new(),
                }
            }

            pub fn add_song(&mut self, song: Song) {
                self.songs.push(song);
            }

            pub fn play_all(&self) {
                for song in &self.songs {
                    song.play();
                }
            }

            pub fn total_songs(&self) -> usize {
                self.songs.len()
            }

            pub fn display_info(&self) {
                println!("Playlist: {} contains {} songs", self.name, self.total_songs());
                for song in &self.songs {
                    println!("Song: {} by {} ({})", song.title, song.artist, song.duration);
                }
            }
        }
    }
}

use library::music::Song;
use library::playlists::Playlist;

fn main() {
    // Create songs using absolute path
    let song1 = library::music::Song::new("Bohemian Rhapsody", "Queen", "5:55");
    let song2 = Song::new("Hotel California", "Eagles", "6:30");

    // Create playlist using imported type
    let mut playlist = Playlist::new("Classic Rock");

    // Add songs to playlist
    playlist.add_song(song1);
    playlist.add_song(song2);

    // Display playlist info
    playlist.display_info();

    // Play all songs
    playlist.play_all();

    // Calculate total duration (simplified)
    println!("Total duration: 12:25");
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
