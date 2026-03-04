package com.treinamaisapi.service.compra.pacote;

import com.treinamaisapi.common.dto.compra.pix.gatewayPix.PixGateway;
import com.treinamaisapi.common.dto.compra.pix.response.CriarCompraPixResponse;
import com.treinamaisapi.common.dto.compra.pix.response.PixCobrancaResponse;
import com.treinamaisapi.common.dto.compra.response.CompraRespondeDireta;
import com.treinamaisapi.common.dto.compra.response.CompraResponse;
import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.common.dto.desconto.CupomPreviewResponse;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.desconto.CupomDesconto;
import com.treinamaisapi.entity.enums.concursos.StatusConcurso;
import com.treinamaisapi.entity.enums.pacotes.MeioPagamento;
import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import com.treinamaisapi.entity.enums.pagamento.StatusReembolso;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.CupomDescontoRepository;
import com.treinamaisapi.repository.PacoteCompradoRepository;
import com.treinamaisapi.repository.PacoteRepository;
import com.treinamaisapi.repository.UsuarioRepository;
import com.treinamaisapi.service.descont.CupomService;
import org.springframework.transaction.annotation.Transactional;  // ✅ Spring
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class PacoteCompradoService {
    private final PacoteCompradoRepository pacoteCompradoRepository;
    private final UsuarioRepository usuarioRepository;
    private final PacoteRepository pacoteRepository;
    private final PixGateway pixGateway;
    private final CupomDescontoRepository cupomDescontoRepository;
    private final CupomService cupomService;




    private PacoteComprado realizarCompra(Long usuarioId, Long pacoteId) {

        Pacote pacote = pacoteRepository.findById(pacoteId)
                .orElseThrow(() -> new RuntimeException("Pacote não encontrado"));

        if (!pacote.isAtivo() ||
                pacote.getConcurso().getStatus() != StatusConcurso.ATIVO) {
            throw new RuntimeException("Pacote indisponível para compra");
        }

        LocalDateTime agora = LocalDateTime.now();

        Optional<PacoteComprado> compraExistenteOpt =
                pacoteCompradoRepository.findByUsuarioIdAndPacoteId(usuarioId, pacoteId);

        // ✅ CASO 1 — Já existe compra
        if (compraExistenteOpt.isPresent()) {
            PacoteComprado compra = compraExistenteOpt.get();

            // 🔒 Já está ativa
            if (compra.getStatus() == StatusCompra.APROVADA && !compra.isExpirado()) {
                throw new RuntimeException("Usuário já possui acesso ativo a este pacote");
            }

            // 🔁 Reativação (CANCELADA ou EXPIRADA)
            compra.setStatus(StatusCompra.APROVADA);
            compra.setAtivo(true);
            compra.setDataCompra(agora);
            compra.setDataExpiracao(agora.plusDays(pacote.getDuracaoDias()));
            compra.setDataCancelamento(null);
            compra.setMotivoCancelamento(null);

            return pacoteCompradoRepository.save(compra);
        }

        // ✅ CASO 2 — Primeira compra
        PacoteComprado novaCompra = PacoteComprado.builder()
                .usuario(usuarioRepository.getReferenceById(usuarioId))
                .pacote(pacote)
                .dataCompra(agora)
                .dataExpiracao(agora.plusDays(pacote.getDuracaoDias()))
                .status(StatusCompra.APROVADA)
                .ativo(true)
                .build();

        return pacoteCompradoRepository.save(novaCompra);
    }





    @Transactional(readOnly = true)
    public List<PacoteCompradoComUsuarioDTO> listarComprasAtivas(Long usuarioId) {
        // Busca todos os pacotes ativos do usuário
        List<PacoteComprado> pacotesAtivos = pacoteCompradoRepository.findByUsuarioIdAndAtivoTrue(usuarioId);

        LocalDate hoje = LocalDate.now();

        // Converte cada PacoteComprado para DTO com cálculo de dias restantes
        return pacotesAtivos.stream()
                .map(pc -> {

                    LocalDate dataProva = pc.getPacote().getConcurso().getDataProva();
                    Long diasRestantes = null;
                    if (dataProva != null) {
                        diasRestantes = ChronoUnit.DAYS.between(hoje, dataProva);
                    }


                    return PacoteCompradoComUsuarioDTO.builder()
                            .compraId(pc.getId())
                            .pacoteId(pc.getPacote().getId())
                            .nomePacote(pc.getPacote().getNome())
                            .dataCompra(pc.getDataCompra())
                            .dataExpiracao(pc.getDataExpiracao())
                            .ativo(pc.isAtivo())
                            .usuarioId(pc.getUsuario().getId())
                            .nomeUsuario(pc.getUsuario().getNome())
                            .emailUsuario(pc.getUsuario().getEmail())
                            .concursoId(pc.getPacote().getConcurso().getId())
                            .nomeConcurso(pc.getPacote().getConcurso().getNome())
                            .dataDaProva(dataProva)
                            .diasRestantes(diasRestantes)
                            .build();
                })
                .toList();
    }


    @Transactional
    public void desativarPacoteExpirado(Long pacoteCompradoId) {
        PacoteComprado pacoteComprado = pacoteCompradoRepository.findById(pacoteCompradoId).orElseThrow(() -> new IllegalArgumentException("Pacote não encontrado."));

        if(pacoteComprado.isExpirado()) {
            pacoteComprado.setAtivo(false);
            pacoteCompradoRepository.save(pacoteComprado);
        }

    }


    // Pagamento por GATeway  meio Pix

    @Transactional
    public CriarCompraPixResponse criarCompraPix(Long usuarioId, Long pacoteId, String codigoCupom) {

        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new BusinessException("Usuário não encontrado"));

        Pacote pacote = pacoteRepository.findById(pacoteId)
                .orElseThrow(() -> new BusinessException("Pacote não encontrado"));

        // 0) normaliza cupom (opcional)
        String cupomNormalizado = (codigoCupom == null || codigoCupom.isBlank())
                ? null
                : codigoCupom.trim().toUpperCase();

        // 1) resolve cupom entidade (opcional)
        CupomDesconto cupom = null;
        if (cupomNormalizado != null) {
            cupom = cupomDescontoRepository.findByCodigoIgnoreCase(cupomNormalizado)
                    .orElseThrow(() -> new BusinessException("Cupom inválido."));
        }

        // 2) calcula valores (sempre no backend)
        BigDecimal precoOriginal = pacote.getPreco().setScale(2, RoundingMode.HALF_UP);

        BigDecimal precoFinal = precoOriginal;
        BigDecimal valorDesconto = BigDecimal.ZERO;

        if (cupom != null) {
            // use um método do CupomService que VALIDA e CALCULA com base no pacote + cupom
            // (você pode usar seu CupomPreviewResponse ou um método direto)
            CupomPreviewResponse preview = cupomService.aplicarDesconto(usuarioId, pacoteId, cupomNormalizado);

            precoFinal = preview.getPrecoFinal().setScale(2, RoundingMode.HALF_UP);
            valorDesconto = preview.getDesconto().setScale(2, RoundingMode.HALF_UP);
        }

        // 3) REUSO: se existe pix pendente e válido, só devolve se for MESMO cupom e MESMO valor
        PacoteComprado existente = pacoteCompradoRepository
                .findTopByUsuarioIdAndPacoteIdAndMeioPagamentoAndStatusInOrderByIdDesc(
                        usuarioId,
                        pacoteId,
                        MeioPagamento.PIX,
                        java.util.List.of(StatusCompra.CRIADA, StatusCompra.PENDENTE)
                )
                .orElse(null);

        if (existente != null
                && existente.getPixExpiracao() != null
                && existente.getPixExpiracao().isAfter(LocalDateTime.now())
                && existente.getPixCopiaCola() != null) {

            Long cupomIdAtual = (cupom != null) ? cupom.getId() : null;
            Long cupomIdExistente = (existente.getCupom() != null) ? existente.getCupom().getId() : null;

            boolean mesmoCupom = java.util.Objects.equals(cupomIdExistente, cupomIdAtual);

            BigDecimal existenteFinal = (existente.getPrecoFinal() != null)
                    ? existente.getPrecoFinal().setScale(2, RoundingMode.HALF_UP)
                    : null;

            boolean mesmoValor = (existenteFinal != null) && existenteFinal.compareTo(precoFinal) == 0;

            if (mesmoCupom && mesmoValor) {
                return CriarCompraPixResponse.builder()
                        .compraId(existente.getId())
                        .status(existente.getStatus())
                        .qrCodeBase64(null)
                        .qrCodeCopiaCola(existente.getPixCopiaCola())
                        .expiracaoPix(existente.getPixExpiracao())
                        .ticketUrl(existente.getPixTicketUrl())
                        .build();
            }

            // opcional (recomendado): expira a pendência anterior para não acumular pendências “inúteis”
            // existente.setStatus(StatusCompra.EXPIRADA);
            // existente.setMotivoCancelamento("Novo PIX gerado com cupom/valor diferente");
            // pacoteCompradoRepository.save(existente);
        }

        // 4) cria compra local com snapshot de valores + cupom (FK)
        PacoteComprado compra = PacoteComprado.builder()
                .usuario(usuario)
                .pacote(pacote)
                .status(StatusCompra.CRIADA)
                .meioPagamento(MeioPagamento.PIX)
                .gateway("MERCADO_PAGO")
                .ativo(false)
                .refundStatus(StatusReembolso.NAO_SOLICITADO)
                .cupom(cupom)                    // ✅ FK
                .precoOriginal(precoOriginal)    // ✅ snapshot
                .precoFinal(precoFinal)          // ✅ snapshot
                .valorDesconto(valorDesconto)    // ✅ snapshot
                .build();

        compra = pacoteCompradoRepository.saveAndFlush(compra);

        String descricaoPagamento = String.format(
                "Treina Mais — %s (%d dias)",
                pacote.getNome(),
                pacote.getDuracaoDias()
        );

        try {
            // 5) cria PIX no gateway com o VALOR FINAL (com desconto)
            PixCobrancaResponse pix = pixGateway.criarCobranca(
                    compra.getId(),
                    precoFinal, // ✅ valor final
                    descricaoPagamento,
                    usuario.getEmail()
            );

            // 6) marca pendente e salva retorno PIX
            compra.setStatus(StatusCompra.PENDENTE);
            compra.setPixTxId(pix.getTxId());
            compra.setPixExpiracao(pix.getExpiracao());
            compra.setPixCopiaCola(pix.getCopiaCola());
            compra.setPixTicketUrl(pix.getTicketUrl());

            pacoteCompradoRepository.save(compra);

            return CriarCompraPixResponse.builder()
                    .compraId(compra.getId())
                    .status(compra.getStatus())
                    .qrCodeBase64(null)
                    .qrCodeCopiaCola(pix.getCopiaCola())
                    .expiracaoPix(pix.getExpiracao())
                    .ticketUrl(pix.getTicketUrl())
                    .build();

        } catch (Exception e) {
            compra.setStatus(StatusCompra.EXPIRADA);
            compra.setRefundErro("Falha ao criar PIX no Mercado Pago: " + e.getMessage());
            pacoteCompradoRepository.save(compra);
            throw e;
        }
    }





    @Transactional
    public void confirmarPagamentoPix(String txId) {

        PacoteComprado compra = pacoteCompradoRepository
                .findByPixTxId(txId)
                .orElseThrow(() -> new BusinessException("Compra não encontrada"));

        if (compra.getStatus() == StatusCompra.APROVADA) {
            return; // idempotente
        }

        compra.setStatus(StatusCompra.APROVADA);
        compra.setAtivo(true);

        LocalDateTime inicio = compra.getDataCompra() != null
                ? compra.getDataCompra()
                : LocalDateTime.now();

        compra.setDataExpiracao(
                inicio.plusDays(
                        compra.getPacote().getDuracaoDias()
                )
        );

        pacoteCompradoRepository.save(compra);
    }


    @Transactional
    public CompraResponse buscarCompra(Long compraId, Long usuarioId) {
        PacoteComprado compra = pacoteCompradoRepository
                .findByIdAndUsuarioId(compraId, usuarioId)
                .orElseThrow(() -> new BusinessException("Compra não encontrada"));

        return CompraResponse.builder()
                .id(compra.getId())
                .status(compra.getStatus())
                .ativo(compra.isAtivo())
                .dataCompra(compra.getDataCompra())
                .dataExpiracao(compra.getDataExpiracao())
                .build();
    }







    @Transactional
    public CompraRespondeDireta comprarSemMeio(Long usuarioId, Long pacoteId) {
        PacoteComprado compra = compraSemGateway(usuarioId, pacoteId);
        return toResponseDireta(compra);
    }

    private PacoteComprado compraSemGateway(Long usuarioId, Long pacoteId) {

        Pacote pacote = pacoteRepository.findById(pacoteId)
                .orElseThrow(() -> new BusinessException("Pacote não encontrado"));

        // (Opcional) Garantir que só pacotes gratuitos possam usar esse fluxo:
    /*
    if (pacote.getPreco() != null && pacote.getPreco().compareTo(BigDecimal.ZERO) > 0) {
        throw new BusinessException("Este pacote exige pagamento via gateway.");
    }
    */

        if (!pacote.isAtivo() ||
                pacote.getConcurso().getStatus() != StatusConcurso.ATIVO) {
            throw new BusinessException("Pacote indisponível para compra");
        }

        LocalDateTime agora = LocalDateTime.now();

        Optional<PacoteComprado> compraExistenteOpt =
                pacoteCompradoRepository.findByUsuarioIdAndPacoteId(usuarioId, pacoteId);

        // ✅ CASO 1 — Já existe compra
        if (compraExistenteOpt.isPresent()) {
            PacoteComprado compra = compraExistenteOpt.get();

            // Já está ativa e não expirada
            if (compra.getStatus() == StatusCompra.APROVADA && !compra.isExpirado()) {
                throw new BusinessException("Usuário já possui acesso ativo a este pacote");
            }

            // 🔁 Reativação (CANCELADA ou EXPIRADA)
            compra.setStatus(StatusCompra.APROVADA);
            compra.setAtivo(true);
            compra.setDataCompra(agora);
            compra.setDataExpiracao(agora.plusDays(pacote.getDuracaoDias()));
            compra.setDataCancelamento(null);
            compra.setMotivoCancelamento(null);

            return pacoteCompradoRepository.save(compra);
        }

        // ✅ CASO 2 — Primeira compra
        PacoteComprado novaCompra = PacoteComprado.builder()
                .usuario(usuarioRepository.getReferenceById(usuarioId))
                .pacote(pacote)
                .dataCompra(agora)
                .dataExpiracao(agora.plusDays(pacote.getDuracaoDias()))
                .status(StatusCompra.APROVADA)// direto aprovado
                .meioPagamento(MeioPagamento.PIX)
                .ativo(true)
                .build();

        return pacoteCompradoRepository.save(novaCompra);
    }

    private CompraRespondeDireta toResponseDireta(PacoteComprado compra) {
        return CompraRespondeDireta.builder()
                .id(compra.getId())
                .pacoteId(compra.getPacote().getId())
                .pacoteNome(compra.getPacote().getNome())
                .valor(compra.getPacote().getPreco())
                .status(compra.getStatus())
                .ativo(compra.isAtivo())
                .dataCompra(compra.getDataCompra())
                .dataExpiracao(compra.getDataExpiracao())
                .build();
    }





}
