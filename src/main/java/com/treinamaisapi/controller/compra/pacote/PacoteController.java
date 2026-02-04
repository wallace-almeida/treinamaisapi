package com.treinamaisapi.controller.compra.pacote;

import com.treinamaisapi.common.dto.compra.response.CompraRespondeDireta;
import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.common.dto.desconto.CriarCupomRequest;
import com.treinamaisapi.common.dto.desconto.CupomPreviewRequest;
import com.treinamaisapi.common.dto.desconto.CupomPreviewResponse;
import com.treinamaisapi.common.dto.desconto.CupomResponse;
import com.treinamaisapi.common.dto.pacote.request.PacoteRequest;
import com.treinamaisapi.common.dto.pacote.response.CatalogoPacoteDTO;
import com.treinamaisapi.common.dto.pacote.response.PacoteResponse;
import com.treinamaisapi.controller.swagger.PacoteControllerSwagger;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.service.compra.pacote.PacoteCompradoService;
import com.treinamaisapi.service.compra.pacote.PacoteService;
import com.treinamaisapi.service.descont.CupomService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/pacotes", produces = MediaType.APPLICATION_JSON_VALUE)
public class PacoteController implements PacoteControllerSwagger {

    private final PacoteCompradoService pacoteCompradoService;
    private final PacoteService pacoteService;
    private final CupomService cupomService;

    @PostMapping("/create")
    @Override
    public ResponseEntity<PacoteResponse> criarPacote(@RequestBody PacoteRequest request) {
        return ResponseEntity.ok(pacoteService.criarPacote(request));
    }

    @GetMapping("/ativos")
    @Override
    public List<PacoteCompradoComUsuarioDTO> listarAtivas(
            @AuthenticationPrincipal Usuario usuario) {

        return pacoteCompradoService.listarComprasAtivas(usuario.getId());
    }


    @PutMapping("/atualizar/{id}")
    @Override
    public PacoteResponse atualizarPacote(@PathVariable Long id, @RequestBody PacoteRequest request) {
        return pacoteService.atualizarPacote(id, request);
    }

    @GetMapping("/{id}/versao")
    @Override
    public Integer getVersao(@PathVariable("id") Long pacoteId) {
        return pacoteService.buscarVersaoPorId(pacoteId);
    }

    @GetMapping("/concurso/{concursoId}")
    @Override
    public ResponseEntity<List<Pacote>> listarPorConcurso(@PathVariable Long concursoId) {
        return ResponseEntity.ok(pacoteService.listarPacotesPorConcurso(concursoId));
    }

    @GetMapping("/catalogo")
    @Override
    public ResponseEntity<List<CatalogoPacoteDTO>> listarCatalogo(
            @AuthenticationPrincipal Usuario usuarioAutenticado
    ) {
        Long usuarioId = (usuarioAutenticado != null) ? usuarioAutenticado.getId() : null;
        return ResponseEntity.ok(pacoteService.listarCatalogo(usuarioId));
    }


    // Compra direta sem passar por gatway

    // ✅ Compra direta (sem meio de pagamento)
    @PostMapping("/pacote/{pacoteId}/compra-direta")
    @Override
    public CompraRespondeDireta comprarPacoteSemMeioPagamento(
            @PathVariable Long pacoteId,
            @AuthenticationPrincipal Usuario usuarioAutenticado) {

        return pacoteCompradoService.comprarSemMeio(usuarioAutenticado.getId(), pacoteId);
    }


    @PostMapping("/cupom/validar")
    @Override
    public CupomPreviewResponse validarCupom(
            @RequestBody CupomPreviewRequest request,
            @AuthenticationPrincipal Usuario usuario) {

        return cupomService.aplicarDesconto(usuario.getId(), request.getPacoteId(), request.getCodigo());
    }

    @PostMapping
    @Override
    public CupomResponse criar(@RequestBody CriarCupomRequest req) {
        return cupomService.criar(req);
    }





}


