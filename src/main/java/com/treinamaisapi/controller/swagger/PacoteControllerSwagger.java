package com.treinamaisapi.controller.swagger;


import com.treinamaisapi.common.dto.compra.response.CompraRespondeDireta;
import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.common.dto.pacote.request.PacoteRequest;
import com.treinamaisapi.common.dto.pacote.response.CatalogoPacoteDTO;
import com.treinamaisapi.common.dto.pacote.response.PacoteResponse;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.usuarios.Usuario;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Pacote", description = " Cadastros dos Pacotes")
public interface PacoteControllerSwagger {


    @PostMapping ("/create")
    ResponseEntity<PacoteResponse> criarPacote(@RequestBody PacoteRequest request);


    @GetMapping("/ativos")
    List<PacoteCompradoComUsuarioDTO> listarAtivas(
            @AuthenticationPrincipal Usuario usuario);

    @PutMapping("/atualizar/{id}")
    PacoteResponse atualizarPacote(@PathVariable Long id, @RequestBody PacoteRequest request);



    @GetMapping("/{id}/versao")
    Integer getVersao(@PathVariable("id") Long pacoteId);

    @GetMapping("/concurso/{concursoId}")
    ResponseEntity<List<Pacote>> listarPorConcurso(@PathVariable Long concursoId);

    @GetMapping("/catalogo")
    ResponseEntity<List<CatalogoPacoteDTO>> listarCatalogo();

    // ✅ Compra direta (sem meio de pagamento)
    @PostMapping("/pacote/{pacoteId}/compra-direta")
    CompraRespondeDireta comprarPacoteSemMeioPagamento(
            @PathVariable Long pacoteId,
            @AuthenticationPrincipal Usuario usuarioAutenticado);
}
