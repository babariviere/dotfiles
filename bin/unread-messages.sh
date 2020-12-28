#!/bin/sh

sqlite3 ~/Library/Messages/chat.db "SELECT COUNT(*) FROM 'message' WHERE is_read = 0 AND text != 'NULL' AND is_from_me != 1;"
