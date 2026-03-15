CREATE INDEX IF NOT EXISTS idx_pacote_expiracao
ON pacotes_comprados (ativo, data_expiracao);