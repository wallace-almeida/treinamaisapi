package com.treinamaisapi.entity.enums.pacotes;

public enum StatusCompra {
    CRIADA,        // criada no sistema
    PENDENTE,      // PIX gerado, aguardando pagamento
    APROVADA,      // pagamento confirmado
    CANCELADA,
    EXPIRADA,
    REEMBOLSADA,
    REEMBOLSO_SOLICITADO
}
