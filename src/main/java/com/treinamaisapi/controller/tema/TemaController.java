package com.treinamaisapi.controller.tema;

import com.treinamaisapi.common.dto.questao.request.EstruturaLoteRequest;
import com.treinamaisapi.common.dto.questao.request.TemaLoteRequest;
import com.treinamaisapi.common.dto.questao.request.TemaRequest;
import com.treinamaisapi.common.dto.questao.response.TemaResponse;
import com.treinamaisapi.controller.swagger.TemaControllerSwagger;
import com.treinamaisapi.service.tema.TemaService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/temas", produces = MediaType.APPLICATION_JSON_VALUE)
public class TemaController implements TemaControllerSwagger {

    private final TemaService temaService;

    @PostMapping("/create")
    @Override
    public ResponseEntity<TemaResponse> criar(@RequestBody TemaRequest request) {
        return ResponseEntity.status(201).body(temaService.criar(request));
    }

    // Inserção em lote simples
    @PostMapping("/lote")
    @Override
    public Map<String, List<String>> criarTemasLote(@RequestBody TemaLoteRequest request) {
        return temaService.criarTemasEmLote(request);
    }

    // Inserção em lote hierárquico
    @PostMapping("/estrutura/lote")
    @Override
    public Map<String, List<String>> criarEstrutura(@RequestBody EstruturaLoteRequest request) {
        return temaService.criarEstruturaCompleta(request);
    }


    @GetMapping
    @Override
    public ResponseEntity<List<TemaResponse>> listar() {
        return ResponseEntity.ok(temaService.listar());
    }

}
