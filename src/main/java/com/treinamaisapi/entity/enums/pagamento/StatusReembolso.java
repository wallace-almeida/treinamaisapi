package com.treinamaisapi.entity.enums.pagamento;

public enum StatusReembolso {
    NAO_APLICAVEL,   // ex: compra pendente cancelada antes de pagar
    NAO_SOLICITADO,
    SOLICITADO,
    CONFIRMADO,
    FALHOU
}
