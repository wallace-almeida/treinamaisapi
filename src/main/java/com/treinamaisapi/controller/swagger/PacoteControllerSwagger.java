package com.treinamaisapi.controller.swagger;


import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.common.dto.pacote.request.PacoteRequest;
import com.treinamaisapi.common.dto.pacote.response.PacoteResponse;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Pacote", description = " Cadastros dos Pacotes")
public interface PacoteControllerSwagger {


    @PostMapping ("/create")
    ResponseEntity<PacoteResponse> criarPacote(@RequestBody PacoteRequest request);


    @GetMapping("/ativos/{usuarioId}")
    List<PacoteCompradoComUsuarioDTO> listarAtivas(@PathVariable Long usuarioId);

    @PutMapping("/atualizar/{id}")
    PacoteResponse atualizarPacote(@PathVariable Long id, @RequestBody PacoteRequest request);



    @GetMapping("/{id}/versao")
    Integer getVersao(@PathVariable("id") Long pacoteId);
}
