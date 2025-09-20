# Hangman Game - React Frontend

A modern React application for the Hangman word-guessing game, built with Vite and featuring a beautiful, responsive UI.

## 🚀 Features

- **Modern React**: Built with React 19 and functional components
- **Fast Development**: Powered by Vite for lightning-fast HMR
- **Responsive Design**: Mobile-first approach with modern CSS
- **Client-Side Routing**: React Router for seamless navigation
- **API Integration**: Axios for backend communication
- **Beautiful UI**: Gradient backgrounds and smooth animations

## 📁 Project Structure

```
src/
├── components/          # React components
│   ├── Home.jsx        # Landing page component
│   └── Home.css        # Home component styles
├── App.jsx             # Main app component with routing
├── App.css             # Global app styles
├── main.jsx            # Application entry point
└── index.css           # Global CSS styles
```

## 🛠️ Setup Instructions

### Prerequisites
- Node.js (v16 or higher)
- npm or yarn

### Installation

1. **Navigate to the client directory:**
   ```bash
   cd client/hangman-client
   ```

2. **Install dependencies:**
   ```bash
   npm install
   ```

3. **Start the development server:**
   ```bash
   npm run dev
   ```

4. **Open your browser:**
   - Navigate to `http://localhost:5173`
   - The app will automatically reload when you make changes

### Available Scripts

- `npm run dev` - Start development server with HMR
- `npm run build` - Build for production
- `npm run preview` - Preview production build locally
- `npm run lint` - Run ESLint for code quality

## 🎨 Current Features

### Home Page (`/`)
- Welcome message and game introduction
- Feature showcase with cards
- Call-to-action buttons for login/register
- Game rules explanation
- Responsive design for all screen sizes

### Routing Structure
- `/` - Home/Landing page
- `/login` - User login (coming soon)
- `/register` - User registration (coming soon)
- `/game` - Main game interface (coming soon)
- `/demo` - Demo mode (coming soon)
- `/profile` - User profile (coming soon)
- `/leaderboard` - Game leaderboard (coming soon)

## 🔗 Backend Integration

The frontend is designed to work with the Node.js/Express backend:

- **API Base URL**: `http://localhost:3001`
- **Authentication**: JWT token-based
- **HTTP Client**: Axios for API requests

### Environment Configuration

Create a `.env` file in the root directory:

```env
VITE_API_URL=http://localhost:3001
VITE_APP_NAME=Hangman Game
```

## 🎮 Game Flow (Planned)

1. **Landing Page** → User sees welcome screen
2. **Authentication** → Login or register
3. **Game Lobby** → Start new game or view history
4. **Gameplay** → Interactive hangman interface
5. **Results** → Score display and statistics

## 📱 Responsive Design

The application is fully responsive and works on:
- Desktop computers (1200px+)
- Tablets (768px - 1199px)
- Mobile phones (320px - 767px)

## 🎨 Design System

### Colors
- **Primary**: `#667eea` (Blue gradient)
- **Secondary**: `#764ba2` (Purple gradient)
- **Background**: `#f7fafc` (Light gray)
- **Text**: `#2d3748` (Dark gray)

### Typography
- **Font Family**: Inter, system fonts
- **Headings**: Bold weights (600-700)
- **Body Text**: Regular weight (400)

### Components
- **Buttons**: Rounded corners, hover effects
- **Cards**: Shadow effects, hover animations
- **Gradients**: Modern CSS gradients throughout

## 🔧 Development Guidelines

### Code Style
- Use functional components with hooks
- Follow React best practices
- Use descriptive component and variable names
- Add comments for complex logic

### File Organization
- Components in `/src/components/`
- Styles alongside components
- Shared utilities in `/src/utils/`
- API calls in `/src/services/`

## 🚀 Deployment

### Build for Production
```bash
npm run build
```

### Preview Production Build
```bash
npm run preview
```

The built files will be in the `dist/` directory, ready for deployment to any static hosting service.

## 🔮 Next Steps

### Immediate Tasks
1. **Authentication Components**: Login and register forms
2. **Game Interface**: Interactive hangman board
3. **API Integration**: Connect with backend endpoints
4. **State Management**: User session and game state

### Future Enhancements
1. **Animations**: Smooth transitions and micro-interactions
2. **Sound Effects**: Audio feedback for game events
3. **Themes**: Dark mode and color customization
4. **Accessibility**: Screen reader support and keyboard navigation

## 🐛 Troubleshooting

### Common Issues

1. **Port already in use:**
   ```bash
   # Kill process on port 5173
   npx kill-port 5173
   ```

2. **Module not found errors:**
   ```bash
   # Clear node_modules and reinstall
   rm -rf node_modules package-lock.json
   npm install
   ```

3. **Build errors:**
   ```bash
   # Check for TypeScript/ESLint errors
   npm run lint
   ```

## 📄 License

This project is part of the Hangman Game educational project.

---

**Ready to create an amazing gaming experience! 🎮**
