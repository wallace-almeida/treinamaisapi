package com.treinamaisapi.controller.compra.pacote;

import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.common.dto.pacote.request.PacoteRequest;
import com.treinamaisapi.common.dto.pacote.response.PacoteResponse;
import com.treinamaisapi.controller.swagger.PacoteControllerSwagger;
import com.treinamaisapi.service.compra.pacote.PacoteCompradoService;
import com.treinamaisapi.service.compra.pacote.PacoteService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/pacotes", produces = MediaType.APPLICATION_JSON_VALUE)
public class PacoteController implements PacoteControllerSwagger {

    private final PacoteCompradoService pacoteCompradoService;
    private final PacoteService pacoteService;

    @PostMapping("/create")
    @Override
    public ResponseEntity<PacoteResponse> criarPacote(@RequestBody PacoteRequest request) {
        return ResponseEntity.ok(pacoteService.criarPacote(request));
    }

    @GetMapping("/ativos/{usuarioId}")
    @Override
    public List<PacoteCompradoComUsuarioDTO> listarAtivas(@PathVariable Long usuarioId) {
        return pacoteCompradoService.listarComprasAtivas(usuarioId);
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
}
