#!/usr/bin/env sh

USERNAME=$(whoami)
HOSTNAME=$(hostname)
RESTIC_REPOSITORY="rclone:proton:backups/$HOSTNAME"
RESTIC_PASSWORD="$(pass show restic)"
LOG_FILE="/home/$USERNAME/backup.log"

# exclude patterns
EXCLUDED_PATTERNS="
    --exclude '.cache/**' 
    --exclude '.local/share/Trash/**' 
    --exclude '.thumbnails/**' 
    --exclude '*.iso' 
    --exclude '*.tmp'
"

export RESTIC_REPOSITORY
export RESTIC_PASSWORD

echo "$(date): Starting Google Drive sync" | tee -a "$LOG_FILE"

# sync using rclone with excluded patterns
rclone sync -vv -P /home/$USERNAME/Documents gdrive:/Backups/$HOSTNAME/Documents $EXCLUDED_PATTERNS | tee -a "$LOG_FILE" # google drive Backups folder capitalized
rclone sync -vv -P /home/$USERNAME/Documents proton:/backups/$HOSTNAME/Documents $EXCLUDED_PATTERNS | tee -a "$LOG_FILE" # proton drive backups folder not capitalized
rclone sync -vv -P /home/$USERNAME/Downloads proton:/backups/$HOSTNAME/Downloads $EXCLUDED_PATTERNS | tee -a "$LOG_FILE"

echo "$(date): Starting Restic backup" | tee -a "$LOG_FILE"

# backup to proton drive using restic with excludes
restic -r -vv "$RESTIC_REPOSITORY" backup \
    /home/"$USERNAME" \
    /etc \
    $EXCLUDED_PATTERNS | tee -a "$LOG_FILE"

echo "$(date): Backup completed" | tee -a "$LOG_FILE"
