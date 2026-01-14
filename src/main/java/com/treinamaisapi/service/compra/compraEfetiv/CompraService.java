package com.treinamaisapi.service.compra.compraEfetiv;

import com.treinamaisapi.common.dto.compra.pix.gatewayPix.PixGateway;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.PacoteCompradoRepository;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class CompraService {

    private static final int PRAZO_CANCELAMENTO_DIAS = 7;

    private final PacoteCompradoRepository repository;

    private final PixGateway pixGateway;

    @Transactional
    public PacoteComprado cancelar(
            Long compraId,
            Usuario usuario,
            String motivo
    ) {

        PacoteComprado compra = repository.findById(compraId)
                .orElseThrow(() -> new BusinessException("Compra não encontrada"));

        // segurança
        if (!compra.getUsuario().getId().equals(usuario.getId())) {
            throw new BusinessException("Você não pode cancelar esta compra");
        }

        // status inválidos
        if (compra.getStatus() == StatusCompra.CANCELADA ||
                compra.getStatus() == StatusCompra.REEMBOLSADA) {
            throw new BusinessException("Compra já cancelada");
        }

        if (compra.getStatus() == StatusCompra.EXPIRADA) {
            throw new BusinessException("Compra expirada");
        }

        // prazo legal
        LocalDateTime limite =
                compra.getDataCompra().plusDays(PRAZO_CANCELAMENTO_DIAS);

        if (LocalDateTime.now().isAfter(limite)) {
            throw new BusinessException("Prazo de cancelamento expirado");
        }

        // regra por status
        if (compra.getStatus() == StatusCompra.PENDENTE ||
                compra.getStatus() == StatusCompra.APROVADA) {

            compra.setStatus(StatusCompra.CANCELADA);
            compra.setAtivo(false);
            compra.setDataCancelamento(LocalDateTime.now());
            compra.setMotivoCancelamento(
                    motivo != null ? motivo : "Cancelamento solicitado pelo usuário"
            );
        }

        return repository.save(compra);
    }
}

