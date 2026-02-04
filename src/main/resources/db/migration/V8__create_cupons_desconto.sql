CREATE TABLE IF NOT EXISTS cupons_desconto (
    id BIGSERIAL PRIMARY KEY,

    codigo VARCHAR(40) NOT NULL UNIQUE,

    -- enum como string (PERCENTUAL | VALOR_FIXO)
    tipo VARCHAR(20) NOT NULL,

    valor NUMERIC(10,2) NOT NULL,

    ativo BOOLEAN NOT NULL DEFAULT TRUE,

    inicio_vigencia TIMESTAMP NULL,
    fim_vigencia TIMESTAMP NULL,

    limite_usos_total INTEGER NULL,
    limite_usos_por_usuario INTEGER NULL,

    valor_minimo_compra NUMERIC(10,2) NULL
);

CREATE INDEX IF NOT EXISTS idx_cupons_desconto_codigo
ON cupons_desconto (codigo);
