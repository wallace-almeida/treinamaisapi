ALTER TABLE pacotes_comprados
    ADD COLUMN IF NOT EXISTS cupom_id BIGINT NULL,
    ADD COLUMN IF NOT EXISTS preco_original NUMERIC(10,2) NULL,
    ADD COLUMN IF NOT EXISTS valor_desconto NUMERIC(10,2) NULL,
    ADD COLUMN IF NOT EXISTS preco_final NUMERIC(10,2) NULL;

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
            REFERENCES cupons_desconto (id)
            ON DELETE SET NULL;
    END IF;
END $$;

CREATE INDEX IF NOT EXISTS idx_pacotes_comprados_cupom_id
ON pacotes_comprados (cupom_id);
