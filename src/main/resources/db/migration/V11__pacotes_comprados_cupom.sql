-- 1) Colunas (idempotente)
ALTER TABLE pacotes_comprados
  ADD COLUMN IF NOT EXISTS cupom_id BIGINT;

ALTER TABLE pacotes_comprados
  ADD COLUMN IF NOT EXISTS preco_original NUMERIC(10,2);

ALTER TABLE pacotes_comprados
  ADD COLUMN IF NOT EXISTS valor_desconto NUMERIC(10,2);

ALTER TABLE pacotes_comprados
  ADD COLUMN IF NOT EXISTS preco_final NUMERIC(10,2);

-- 2) FK (idempotente no Postgres via check no catalog)
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_constraint
    WHERE conname = 'fk_pacotes_comprados_cupom'
  ) THEN
    ALTER TABLE pacotes_comprados
      ADD CONSTRAINT fk_pacotes_comprados_cupom
      FOREIGN KEY (cupom_id)
      REFERENCES cupons_desconto(id);
  END IF;
END $$;

-- 3) Index (opcional mas recomendado)
CREATE INDEX IF NOT EXISTS idx_pacotes_comprados_cupom_id
  ON pacotes_comprados(cupom_id);
