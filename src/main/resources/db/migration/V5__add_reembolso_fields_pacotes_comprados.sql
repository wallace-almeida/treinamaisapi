-- 1) cria como NULLável
ALTER TABLE pacotes_comprados
ADD COLUMN refund_status VARCHAR(30);

-- 2) preenche todos os registros antigos
UPDATE pacotes_comprados
SET refund_status = CASE
    WHEN status = 'REEMBOLSADA' THEN 'CONFIRMADO'
    ELSE 'NAO_SOLICITADO'
END
WHERE refund_status IS NULL;

-- 3) garante default para novos registros (opcional, mas recomendado)
ALTER TABLE pacotes_comprados
ALTER COLUMN refund_status SET DEFAULT 'NAO_SOLICITADO';

-- 4) agora sim trava como NOT NULL
ALTER TABLE pacotes_comprados
ALTER COLUMN refund_status SET NOT NULL;

-- 5) demais colunas
ALTER TABLE pacotes_comprados
ADD COLUMN refund_id VARCHAR(100),
ADD COLUMN refund_valor NUMERIC(15,2),
ADD COLUMN refund_solicitado_em TIMESTAMP,
ADD COLUMN refund_confirmado_em TIMESTAMP,
ADD COLUMN refund_erro VARCHAR(500);
