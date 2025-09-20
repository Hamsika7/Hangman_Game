# Hangman Game Backend Server

A robust Node.js/Express backend server for the Hangman game application with SQLite database, user authentication, and game state management.

## 🚀 Quick Start

### Prerequisites
- Node.js (v14 or higher)
- npm (comes with Node.js)

### Installation & Setup

1. **Navigate to server directory:**
   ```bash
   cd server
   ```

2. **Install dependencies:**
   ```bash
   npm install
   ```

3. **Start development server:**
   ```bash
   npm run dev
   ```

4. **Or start production server:**
   ```bash
   npm start
   ```

5. **Server will be running at:**
   - Main API: `http://localhost:3001`
   - Health check: `http://localhost:3001/health`
   - API docs: `http://localhost:3001/api`

## 📁 Project Structure

```
server/
├── db/
│   ├── database.js      # SQLite connection and utilities
│   ├── init.sql         # Database schema and initial data
│   └── hangman.db       # SQLite database file (auto-created)
├── public/              # Static files (optional)
├── server.js            # Main Express server
├── package.json         # Dependencies and scripts
├── .gitignore          # Git ignore rules
└── README.md           # This file
```

## 🗄️ Database Schema

### Users Table
- `id` - Primary key (auto-increment)
- `username` - Unique username
- `password_hash` - Bcrypt hashed password
- `score` - Total user score
- `games_played` - Number of games played
- `games_won` - Number of games won
- `games_lost` - Number of games lost
- `created_at` - Account creation timestamp
- `updated_at` - Last update timestamp

### Games Table
- `id` - Primary key (auto-increment)
- `user_id` - Foreign key to users table
- `word` - The secret word for the game
- `masked_word` - Current masked state (e.g., "_a_a_e")
- `guessed_letters` - All letters guessed so far
- `incorrect_guesses` - Wrong letters guessed
- `attempts_left` - Remaining incorrect guesses allowed
- `status` - Game status: 'active', 'won', 'lost', 'abandoned'
- `score` - Points earned for this game
- `started_at` - Game start timestamp
- `completed_at` - Game completion timestamp

## 🔧 Available Scripts

- `npm start` - Start production server
- `npm run dev` - Start development server with nodemon
- `npm run init-db` - Manually initialize database
- `npm test` - Run tests (not implemented yet)

## 🌐 API Endpoints

### Current Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/` | API welcome and info |
| GET | `/health` | Server health check |
| GET | `/api` | API documentation |

### Planned Endpoints (Coming Soon)

#### Authentication
- `POST /api/auth/register` - User registration
- `POST /api/auth/login` - User login
- `GET /api/auth/profile` - Get user profile
- `POST /api/auth/logout` - User logout

#### Game Management
- `POST /api/games/start` - Start new game
- `GET /api/games/:id` - Get game status
- `POST /api/games/:id/guess` - Make a letter guess
- `PUT /api/games/:id/abandon` - Abandon current game
- `GET /api/games/history` - Get user's game history

#### User Statistics
- `GET /api/users/stats` - Get user statistics
- `GET /api/users/leaderboard` - Get top players

## 🔒 Security Features

- **Password Hashing**: Uses bcrypt for secure password storage
- **JWT Authentication**: JSON Web Tokens for session management
- **CORS Protection**: Configured for cross-origin requests
- **Input Validation**: Request body parsing with size limits
- **SQL Injection Protection**: Parameterized queries

## 🛠️ Dependencies

### Production Dependencies
- `express` - Web application framework
- `sqlite3` - SQLite database driver
- `cors` - Cross-Origin Resource Sharing
- `bcrypt` - Password hashing
- `jsonwebtoken` - JWT token generation/verification
- `dotenv` - Environment variable management

### Development Dependencies
- `nodemon` - Auto-restart server on file changes

## 🌍 Environment Variables

Create a `.env` file in the server directory:

```env
# Server Configuration
PORT=3001
NODE_ENV=development

# Database
DB_PATH=./db/hangman.db

# Authentication
JWT_SECRET=your-super-secret-jwt-key-change-this-in-production
JWT_EXPIRES_IN=24h

# CORS
CLIENT_URL=http://localhost:3000

# Security
BCRYPT_ROUNDS=10
```

## 🔄 Database Operations

The database is automatically initialized when the server starts. You can also manually initialize it:

```bash
npm run init-db
```

### Manual Database Access

You can access the SQLite database directly:

```bash
# Install sqlite3 CLI (if not already installed)
# Windows: Download from https://sqlite.org/download.html
# macOS: brew install sqlite
# Linux: sudo apt-get install sqlite3

# Access database
sqlite3 db/hangman.db

# Example queries
.tables
SELECT * FROM users;
SELECT * FROM games WHERE status = 'active';
```

## 📊 Monitoring & Logging

- Request logging with timestamps and IP addresses
- Error logging with stack traces (development mode)
- Health check endpoint for monitoring
- Graceful shutdown handling

## 🚀 Deployment

### Development
```bash
npm run dev
```

### Production
```bash
npm start
```

### Docker (Future Enhancement)
```dockerfile
# Example Dockerfile structure
FROM node:16-alpine
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY . .
EXPOSE 3001
CMD ["npm", "start"]
```

## 🧪 Testing

Testing framework will be added in future development phases. Planned testing includes:

- Unit tests for database operations
- Integration tests for API endpoints
- Authentication flow testing
- Game logic testing

## 🔧 Troubleshooting

### Common Issues

1. **Port already in use:**
   ```bash
   # Change PORT in .env file or:
   PORT=3002 npm start
   ```

2. **Database permission errors:**
   ```bash
   # Ensure write permissions in db/ directory
   chmod 755 db/
   ```

3. **Module not found errors:**
   ```bash
   # Reinstall dependencies
   rm -rf node_modules package-lock.json
   npm install
   ```

## 📈 Future Enhancements

- [ ] Complete API endpoint implementation
- [ ] WebSocket support for real-time gameplay
- [ ] Rate limiting and API throttling
- [ ] Comprehensive logging system
- [ ] Docker containerization
- [ ] Unit and integration tests
- [ ] API documentation with Swagger
- [ ] Database migrations system
- [ ] Redis caching layer
- [ ] Monitoring and metrics

## 🤝 Contributing

1. Follow existing code style and conventions
2. Add comprehensive comments for new features
3. Update this README for any new functionality
4. Test thoroughly before committing

## 📝 License

This project is private and for educational purposes.

---

**Ready to build an amazing Hangman game! 🎮**
