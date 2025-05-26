#!/usr/bin/env sh

# Set variables
USERNAME=$(whoami)
HOSTNAME=$(hostname)
RESTIC_REPOSITORY="rclone:protondrive:backups/nineveh"
RESTIC_PASSWORD="$(pass show restic)"
LOG_FILE="/home/$USERNAME/backup.log"  # Update this to your desired log file path

# Export necessary variables for Restic
export RESTIC_REPOSITORY
export RESTIC_PASSWORD

# Start the backup
echo "$(date): Starting backup" | tee -a "$LOG_FILE"

restic -r "$RESTIC_REPOSITORY" backup \
    /home/"$USERNAME" \
    /etc \
    /usr/local/bin \
    --exclude="/home/$USERNAME/.cache" \
    --exclude="/home/$USERNAME/.local/share/Trash" \
    --exclude="/home/$USERNAME/Downloads" \
    --exclude="/home/$USERNAME/.thumbnails" \
    --exclude="*.iso" \
    --exclude="*.tmp" | tee -a "$LOG_FILE"                            

echo "$(date): Backup completed" | tee -a "$LOG_FILE"
