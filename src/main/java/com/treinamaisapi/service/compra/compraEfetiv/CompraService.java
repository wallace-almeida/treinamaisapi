package com.treinamaisapi.service.compra.compraEfetiv;

import com.treinamaisapi.common.dto.cancelamentoCompra.MpRefundResponse;
import com.treinamaisapi.common.dto.compra.pix.gatewayPix.MpPaymentStatusResponse;
import com.treinamaisapi.common.dto.compra.pix.gatewayPix.PixGateway;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import com.treinamaisapi.entity.enums.pagamento.StatusReembolso;
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
    public PacoteComprado cancelar(Long compraId, Usuario usuario, String motivo) {

        PacoteComprado compra = repository.findById(compraId)
                .orElseThrow(() -> new BusinessException("Compra não encontrada"));

        if (!compra.getUsuario().getId().equals(usuario.getId())) {
            throw new BusinessException("Você não pode cancelar esta compra");
        }

        if (!compra.podeCancelar()) {
            throw new BusinessException("Prazo de cancelamento expirado");
        }

        if (compra.getPixTxId() == null) {
            throw new BusinessException("Compra não possui pagamento PIX associado");
        }

        // consulta no MP (status do pagamento)
        MpPaymentStatusResponse mp = pixGateway.buscarPagamento(compra.getPixTxId());
        String mpStatus = mp != null ? mp.getStatus() : null;

        // aplica cancelamento local (corta acesso imediatamente)
        compra.setAtivo(false);
        compra.setDataCancelamento(LocalDateTime.now());
        compra.setMotivoCancelamento(
                motivo != null ? motivo : "Cancelamento solicitado pelo usuário"
        );

        if ("approved".equalsIgnoreCase(mpStatus)) {

            // reembolso total
            MpRefundResponse refund = pixGateway.reembolsarPagamento(compra.getPixTxId(), null);

            compra.setRefundId(refund.id());
            compra.setRefundValor(refund.amount());
            compra.setRefundSolicitadoEm(LocalDateTime.now());
            compra.setRefundErro(null);

            // se o reembolso já veio aprovado (bem comum no MP)
            if ("approved".equalsIgnoreCase(refund.status())) {
                compra.setStatus(StatusCompra.REEMBOLSADA);
                compra.setRefundStatus(StatusReembolso.CONFIRMADO);
                compra.setRefundConfirmadoEm(LocalDateTime.now());
            } else {
                compra.setStatus(StatusCompra.REEMBOLSO_SOLICITADO);
                compra.setRefundStatus(StatusReembolso.SOLICITADO);
            }

        } else if ("pending".equalsIgnoreCase(mpStatus) ||
                "in_process".equalsIgnoreCase(mpStatus) ||
                "authorized".equalsIgnoreCase(mpStatus)) {

            pixGateway.cancelarCobranca(compra.getPixTxId());

            compra.setStatus(StatusCompra.CANCELADA);
            compra.setRefundStatus(StatusReembolso.NAO_APLICAVEL);
            compra.setRefundErro(null);

        } else if ("cancelled".equalsIgnoreCase(mpStatus)) {

            compra.setStatus(StatusCompra.CANCELADA);
            compra.setRefundStatus(StatusReembolso.NAO_APLICAVEL);

        } else if ("refunded".equalsIgnoreCase(mpStatus)) {

            // pagamento já está como refunded no MP
            compra.setStatus(StatusCompra.REEMBOLSADA);
            compra.setRefundStatus(StatusReembolso.CONFIRMADO);
            if (compra.getRefundConfirmadoEm() == null) {
                compra.setRefundConfirmadoEm(LocalDateTime.now());
            }

        } else {
            throw new BusinessException("Status do pagamento não permite cancelamento/reembolso: " + mpStatus);
        }

        return repository.save(compra);
    }
    private String trunc(String s, int max) {
        if (s == null) return null;
        return s.length() <= max ? s : s.substring(0, max);
    }

}

