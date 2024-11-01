-- mysql -u root -p

-- Step 1: create a new database to host users info
CREATE DATABASE IF NOT EXISTS user_management;

-- Step 2: Use the database
USE user_management;

-- Step 3: Create the User Table
CREATE TABLE IF NOT EXISTS users (
  id INT AUTO_INCREMENT PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE,
  password_hash VARCHAR(255) NOT NULL,
  activation_token VARCHAR(255),
  is_active BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS logins (
  id INT AUTO_INCREMENT PRIMARY KEY,
  user_id INT,
  login_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

