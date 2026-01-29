-- =========================================================
-- Migration: adiciona dados persistentes do PIX
-- =========================================================

ALTER TABLE pacotes_comprados
ADD COLUMN pix_copia_cola TEXT,
ADD COLUMN pix_ticket_url TEXT;
