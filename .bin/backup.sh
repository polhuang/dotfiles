#!/usr/bin/env sh

# Set variables
USERNAME=$(whoami)
HOSTNAME=$(hostname)
RESTIC_REPOSITORY="rclone:protondrive:backups/nineveh"
RESTIC_PASSWORD_FILE="$(pass show restic)"
LOG_FILE="/path/to/backup.log"  # Update this to your desired log file path

# Export necessary variables for Restic
export RESTIC_REPOSITORY
export RESTIC_PASSWORD_FILE

# Start the backup
echo "$(date): Starting backup" | tee -a "$LOG_FILE"

restic -r "$RESTIC_REPOSITORY" backup \
    /home/"$USERNAME" \
    /etc \
    /usr/local/bin | tee -a "$LOG_FILE"

echo "$(date): Backup completed" | tee -a "$LOG_FILE"
