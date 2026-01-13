package com.treinamaisapi.controller.compra.concurso;

import com.treinamaisapi.common.dto.concurso.request.ConcursoRequest;
import com.treinamaisapi.common.dto.concurso.response.ConcursoResponse;
import com.treinamaisapi.common.dto.concurso.response.ConcursoStatusResponse;
import com.treinamaisapi.controller.swagger.ConcursoControllerSwagger;
import com.treinamaisapi.service.compra.concurso.ConcursoService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/concurso", produces = MediaType.APPLICATION_JSON_VALUE)
public class ConcursoController implements ConcursoControllerSwagger {

    private final ConcursoService concursoService;



    @PostMapping("/create")
    @Override
    public ResponseEntity<ConcursoResponse> criarConcurso(@RequestBody ConcursoRequest request) {
        return ResponseEntity.ok(concursoService.criarConcurso(request));
    }



    @GetMapping("/{id}")
    @Override
    public ResponseEntity<ConcursoResponse> buscarPorId(@PathVariable Long id) {
        return ResponseEntity.ok(concursoService.buscarPorId(id));
    }



    @PatchMapping("/{concursoId}/ativar")
    @Override
    public ConcursoStatusResponse ativar(@PathVariable Long concursoId) {
        return concursoService.ativarConcurso(concursoId);
    }

    @PatchMapping("/{concursoId}/encerrar")
    @Override
    public ConcursoResponse encerrar(@PathVariable Long concursoId) {
        return concursoService.encerrarConcurso(concursoId);
    }





}
